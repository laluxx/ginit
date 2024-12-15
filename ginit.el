;;; ginit.el --- GitHub repository initialization with submodules -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Laluxx

;; Author: Laluxx
;; Version: 0.5
;; Package-Requires: ((emacs "27.1") (spinner "1.7.4"))
;; Keywords: git, github, version control
;; URL: https://github.com/laluxx/ginit

;;; Commentary:

;; [X] DONE Auto suggest the repo name based on the file name without extension
;; [X] DONE Auto generate a readme.org, if it is an emacs package use the name of the package in the readme
;; [X] DONE Improve message formatting with syntax highlighting

;; This package provides a function to initialize a Git repository,
;; create it on GitHub, and handle submodules automatically.

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'spinner)

(defgroup ginit nil
  "Customization group for ginit."
  :group 'tools)

(defcustom ginit-github-username "laluxx"
  "GitHub username for repository creation."
  :type 'string
  :group 'ginit)

(defvar ginit-spinner (spinner-create 'moon)
  "Spinner for ginit operations.")

(defun ginit-format-message (status message &rest args)
  "Format MESSAGE with ARGS and add STATUS prefix with appropriate face."
  (let* ((status-face (pcase status
                        ('success 'success)
                        ('warning 'warning)
                        ('error 'error)
                        (_ 'default)))
         (formatted-message (apply #'format message args))
         (status-string (propertize (symbol-name status) 'face status-face))
         (highlighted-message (with-temp-buffer
                                (insert formatted-message)
                                (delay-mode-hooks
                                  (emacs-lisp-mode)
                                  (font-lock-ensure))
                                (buffer-string))))
    (format "[%s] %s" status-string highlighted-message)))

(defun ginit-message (status message &rest args)
  "Display a formatted message with STATUS and syntax highlighting."
  (message "%s" (apply #'ginit-format-message status message args)))

(defun ginit-suggest-repo-name ()
  "Suggest a repository name based on the current directory contents."
  (let* ((el-files (directory-files "." t "\\.el$"))
         (license-file (or (file-exists-p "LICENSE") (file-exists-p "LICENSE.txt")))
         (suggested-name (when (and el-files license-file)
                           (file-name-sans-extension (file-name-nondirectory (car el-files))))))
    (read-string "Enter repository name: " suggested-name)))

(defun ginit-generate-readme (repo-name)
  "Generate a README.org file for the repository REPO-NAME."
  (let* ((el-files (directory-files "." t "\\.el$"))
         (license-file (or (file-exists-p "LICENSE") (file-exists-p "LICENSE.txt")))
         (is-package (and el-files license-file)))
    (when is-package
      (with-temp-file "README.org"
        (insert (format "#+TITLE: %s\n" repo-name))
        (insert (format "#+AUTHOR: %s\n\n" ginit-github-username))
        (insert "* Install\n")
        (insert "Clone it\n")
        (insert (format "#+begin_src shell\ngit clone https://github.com/%s/%s ~/.config/emacs/lisp/%s\n#+end_src\n\n"
                        ginit-github-username repo-name repo-name))
        (insert "Add to init.el\n")
        (insert "#+begin_src elisp\n")
        (insert (format "(use-package %s\n  :load-path \"~/.config/emacs/lisp/%s\")\n"
                        repo-name repo-name))
        (insert "#+end_src\n")))))

(defun ginit-run-command (&rest args)
  "Run a command with ARGS and signal an error if it fails."
  (unless (zerop (apply #'call-process (car args) nil nil nil (cdr args)))
    (error "Command failed: %s" (mapconcat #'identity args " "))))

(defun ginit-add-submodules (original-dir)
  "Add submodules from the current directory."
  (dolist (dir (directory-files-recursively "." "^\\.git$" t))
    (let* ((submodule-path (file-name-directory (directory-file-name dir)))
           (default-directory submodule-path)
           (remote-url (string-trim (shell-command-to-string "git config --get remote.origin.url"))))
      (if (not (string-empty-p remote-url))
          (progn
            (setq default-directory original-dir)
            (ginit-message 'success "Adding submodule: %s -> %s" (file-relative-name submodule-path) remote-url)
            (ginit-run-command "git" "submodule" "add" remote-url (file-relative-name submodule-path)))
        (ginit-message 'warning "No remote URL found for %s" (file-relative-name submodule-path))))))

(defun ginit-directory-size (dir)
  "Calculate the total size of DIR in bytes."
  (seq-reduce #'+ (mapcar (lambda (file)
                            (if (file-directory-p file)
                                (ginit-directory-size file)
                              (file-attribute-size (file-attributes file))))
                          (directory-files dir t "^[^.]"))
              0))

;;;###autoload
(defun ginit ()
  "Initialize a Git repository and create it on GitHub with submodules."
  (interactive)
  (let* ((repo-name (ginit-suggest-repo-name))
         (original-dir default-directory)
         (commit-message "initial commit")
         (branch-name "main"))
    
    (spinner-start ginit-spinner)
    
    (condition-case err
        (progn
          ;; Check for required commands
          (unless (and (executable-find "gh") (executable-find "git"))
            (error "The 'gh' and 'git' commands are required"))

          ;; Check repository name
          (when (string-empty-p repo-name)
            (error "Repository name is empty"))

          ;; Check directory size
          (when (> (ginit-directory-size ".") (* 2 1024 1024 1024))
            (error "The directory size exceeds 2 GB"))

          ;; Generate README.org
          (ginit-generate-readme repo-name)
          (ginit-message 'success "Generated README.org for %s" repo-name)

          ;; Create repository on GitHub
          (ginit-message 'success "Creating repository %s on GitHub..." repo-name)
          (ginit-run-command "gh" "repo" "create" repo-name "--public")

          ;; Initialize local Git repository
          (ginit-message 'success "Initializing local Git repository...")
          (ginit-run-command "git" "init")

          ;; Scan for existing Git repositories and add as submodules
          (ginit-message 'success "Scanning for existing Git repositories...")
          (ginit-add-submodules original-dir)

          ;; Add all files to staging area
          (ginit-message 'success "Adding all files to the staging area...")
          (ginit-run-command "git" "add" ".")

          ;; Commit changes
          (ginit-message 'success "Committing changes...")
          (ginit-run-command "git" "commit" "-m" commit-message)

          ;; Rename default branch
          (ginit-message 'success "Renaming default branch to %s..." branch-name)
          (ginit-run-command "git" "branch" "-M" branch-name)

          ;; Set remote origin
          (ginit-message 'success "Setting remote origin to GitHub repository...")
          (ginit-run-command "git" "remote" "add" "origin"
                             (concat "https://github.com/" ginit-github-username "/" repo-name ".git"))

          ;; Push changes to remote repository
          (ginit-message 'success "Pushing changes to remote repository...")
          (ginit-run-command "git" "push" "-u" "origin" branch-name)

          ;; Initialize and update submodules
          (ginit-message 'success "Initializing and updating submodules...")
          (ginit-run-command "git" "submodule" "update" "--init" "--recursive")

          (ginit-message 'success "Done! Repository %s created with submodules." repo-name))
      
      (error
       (ginit-message 'error "%s" (error-message-string err))))
    
    (spinner-stop ginit-spinner)))

(provide 'ginit)

;;; ginit.el ends here
