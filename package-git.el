;; package-git.el --- Git-based package management for ELPA

;;; Commentary:
;; This package provides Git-based version control for ELPA packages,
;; allowing rollback to previous package versions.

;;; Code:

(require 'package)

(defgroup package-git nil
  "Git-based package management for ELPA."
  :group 'package)

(defcustom package-git-auto-commit t
  "Whether to automatically commit package changes."
  :type 'boolean
  :group 'package-git)

(defvar package-git--batch-operation nil
  "Flag to indicate batch operation in progress (suppresses individual commits).")

(defvar package-git--batch-changes nil
  "List of changes during batch operation.")

(defun package-git--git-available-p ()
  "Check if Git is available on the system."
  (condition-case nil
      (progn
        (call-process "git" nil nil nil "--version")
        t)
    (error
     nil)))

(defun package-git--ensure-git-repo ()
  "Ensure git repository exists in package-user-dir."
  (let ((default-directory package-user-dir))
    (unless (file-directory-p ".git")
      (message "Initializing Git repository in %s" package-user-dir)
      (shell-command "git init")
      (shell-command "git config user.name 'ELPA Git Manager'")
      (shell-command "git config user.email 'elpa@localhost'")
      ;; Create .gitignore for common files to ignore
      (with-temp-file ".gitignore"
        (insert "*.elc\n")
        (insert "*~\n")
        (insert "archives/\n")
        (insert "gnupg/\n"))
      ;; If packages already exist, commit them
      (when (directory-files "." nil "^[^.]")
        (message "Committing existing packages...")
        (shell-command "git add .")
        (shell-command
         (format "git commit -m %s"
                 (shell-quote-argument
                  "Initial commit: existing packages")))))))

(defun package-git--commit-changes (message)
  "Commit changes with MESSAGE if there are any."
  (let ((default-directory package-user-dir))
    (package-git--ensure-git-repo)
    (when (> (length
              (shell-command-to-string "git status --porcelain"))
             0)
      (shell-command "git add .")
      ;; Use shell-quote-argument to properly escape the message
      (shell-command
       (format "git commit -m %s" (shell-quote-argument message)))
      (message "Committed package changes: %s" message))))

(defun package-git--schedule-commit (operation packages)
  "Schedule a commit for OPERATION on PACKAGES."
  (when package-git-auto-commit
    (if package-git--batch-operation
        ;; During batch operation, just record the change
        (push (list operation packages) package-git--batch-changes)
      ;; Normal operation - commit immediately (synchronously)
      (let ((pkg-names
             (if (listp packages)
                 (string-join (remove nil packages) ", ")
               packages)))
        (package-git--commit-changes
         (format "%s: %s" operation pkg-names))))))

(defun package-git--start-batch-operation (operation-name)
  "Start a batch operation with OPERATION-NAME."
  (setq package-git--batch-operation operation-name)
  (setq package-git--batch-changes nil))

(defun package-git--create-batch-commit-message (operation-name changes)
  "Create commit message for batch OPERATION-NAME with CHANGES."
  (if (string= operation-name "Package upgrade")
      (let ((packages
             (delete-dups
              (flatten-list
               (mapcar
                (lambda (change)
                  (let ((pkg (cadr change)))
                    (if (listp pkg)
                        pkg
                      (list pkg))))
                changes)))))
        (format "Package upgrade: %s" (string-join packages ", ")))
    (format "%s: multiple operations" operation-name)))

(defun package-git--end-batch-operation ()
  "End batch operation and commit all changes."
  (when package-git--batch-operation
    (let* ((operation-name package-git--batch-operation)
           (changes package-git--batch-changes)
           (commit-message
            (package-git--create-batch-commit-message
             operation-name changes)))
      (setq package-git--batch-operation nil)
      (setq package-git--batch-changes nil)
      (when changes
        (package-git--commit-changes commit-message)))))

(defun package-git--get-package-name (pkg)
  "Extract package name from PKG (symbol or package-desc)."
  (cond
   ((symbolp pkg)
    (symbol-name pkg))
   ((package-desc-p pkg)
    (symbol-name (package-desc-name pkg)))
   (t
    "unknown")))

;; Advice functions
(defun package-git--advice-package-install (orig-fun &rest args)
  "Advice for package install functions."
  (let ((pkg-name (package-git--get-package-name (car args)))
        (result (apply orig-fun args)))
    (package-git--schedule-commit "Install" (list pkg-name))
    result))

(defun package-git--advice-package-delete (orig-fun &rest args)
  "Advice for package delete functions."
  (let ((pkg-name (package-git--get-package-name (car args)))
        (result (apply orig-fun args)))
    (package-git--schedule-commit "Delete" (list pkg-name))
    result))

(defun package-git--advice-package-upgrade-all (orig-fun &rest args)
  "Advice for package-upgrade-all."
  (package-git--start-batch-operation "Package upgrade")
  (let ((result (apply orig-fun args)))
    (package-git--end-batch-operation)
    result))

(defun package-git--advice-package-upgrade (orig-fun &rest args)
  "Advice for individual package upgrade."
  (package-git--start-batch-operation "Package upgrade")
  (let ((result (apply orig-fun args)))
    (package-git--end-batch-operation)
    result))

(defun package-git--advice-package-menu-execute (orig-fun &rest args)
  "Advice for package-menu-execute."
  (package-git--start-batch-operation "Package menu operations")
  (let ((result (apply orig-fun args)))
    (package-git--end-batch-operation)
    result))

;;;###autoload
(defun package-git-enable ()
  "Enable Git management for ELPA packages."
  (interactive)
  ;; Check if Git is available
  (unless (package-git--git-available-p)
    (user-error
     "Git is not available on this system. Cannot enable ELPA Git management"))

  ;; Ensure package-user-dir exists
  (unless (file-directory-p package-user-dir)
    (message "Creating package directory: %s" package-user-dir)
    (make-directory package-user-dir t))

  (package-git--ensure-git-repo)

  ;; Add advice to package functions
  (advice-add
   'package-install
   :around #'package-git--advice-package-install)
  (advice-add
   'package-delete
   :around #'package-git--advice-package-delete)
  (advice-add
   'package-upgrade-all
   :around #'package-git--advice-package-upgrade-all)

  ;; Handle individual package upgrades if the function exists
  (when (fboundp 'package-upgrade)
    (advice-add
     'package-upgrade
     :around #'package-git--advice-package-upgrade))

  ;; Handle package-menu operations
  (advice-add
   'package-menu-execute
   :around #'package-git--advice-package-menu-execute)

  (message "ELPA Git management enabled"))

;;;###autoload
(defun package-git-disable ()
  "Disable Git management for ELPA packages."
  (interactive)
  (advice-remove 'package-install #'package-git--advice-package-install)
  (advice-remove 'package-delete #'package-git--advice-package-delete)
  (advice-remove
   'package-upgrade-all #'package-git--advice-package-upgrade-all)

  (when (fboundp 'package-upgrade)
    (advice-remove
     'package-upgrade #'package-git--advice-package-upgrade))

  (advice-remove
   'package-menu-execute #'package-git--advice-package-menu-execute)
  (message "ELPA Git management disabled"))

;;;###autoload
(defun package-git-commit-now (message)
  "Manually commit current package state with MESSAGE."
  (interactive "sCommit message: ")
  (package-git--commit-changes message))

(provide 'package-git)

;;; package-git.el ends here
