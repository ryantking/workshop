;;; vftc-project.el --- VFTC Emacs Project Utilities -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Some utility functions from prot.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'vc)

(defgroup vftc-project ()
  "Extensions for project.el and related libraries."
  :group 'project)

(defcustom vftc-project-project-roots (list "~/Projects")
  "List of directories with version-controlled projects.
To be used by `vftc-project-switch-project'."
  :type 'list
  :group 'vftc-project)

(defcustom vftc-project-commit-log-limit 25
  "Limit commit logs for project to N entries by default.
A value of 0 means 'unlimited'."
  :type 'integer
  :group 'vftc-project)

(defcustom vftc-project-large-file-lines 1000
  "How many lines constitute a 'large file' (integer).
This determines whether some automatic checks should be executed
or not, such as `vftc-project-flymake-mode-activate'."
  :type 'integer
  :group 'vtfc-project)

(cl-defmethod project-root ((project (head local)))
  "Project root for PROJECT with HEAD and LOCAL."
  (cdr project))

(defun vftc-project--project-files-in-directory (dir)
  "Use `fd' to list files in DIR."
  (unless (executable-find "fd")
    (error "Cannot find 'fd' command is shell environment $PATH"))
  (let* ((default-directory dir)
         (localdir (file-local-name (expand-file-name dir)))
         (command (format "fd -t f -0 . %s" localdir)))
    (project--remote-file-names
     (split-string (shell-command-to-string command) "\0" t))))


(cl-defmethod project-files ((project (head local)) &optional dirs)
  "Override `project-files' to use `fd' in local projects.

Project root for PROJECT with HEAD and LOCAL, plus optional
DIRS."
  (mapcan #'prot-project--project-files-in-directory
          (or dirs (list (project-root project)))))

(defun vftc-project--list-projects ()
  "Produce list of projects in `vftc-project-project-roots'."
  (let* ((dirs vftc-project-project-roots)
         (dotless directory-files-no-dot-files-regexp)
         (cands (mapcan (lambda (d)
                          (directory-files d t dotless))
                        dirs)))
    (mapcar (lambda (d)
              (list (abbreviate-file-name d)))
            cands)))

;;;###autoload
(defun vftc-project-add-projects ()
  "Append `vftc-project--list-projects' to `project--list'."
  (interactive)
  (project--ensure-read-project-list)
  (let ((projects (vftc-project--list-projects)))
    (setq project--list (append projects project--list))
    (project--write-project-list)))

;;;###autoload
(defun vftc-project-remove-project ()
  "Remove project from `project--list' using completion."
  (interactive)
  (project--ensure-read-project-list)
  (let* ((projects project--list)
         (dir (completing-read "REMOVE project from list: " projects nil t)))
    (setq project--list (delete (assoc dir projects) projects))
    (project--write-project-list)))

(defun vftc-project--directory-subdirs (dir)
  "Return list of subdirectories in DIR."
  (cl-remove-if-not
   (lambda (x)
     (file-directory-p x))
   (directory-files-recursively dir ".*" t t)))

(defun vftc-project--directory-subdirs-no-git (dir)
  "Remove .git dirs from DIR."
  (cl-remove-if
   (lambda (x)
     (string-match-p "\\.git" x))
   (vftc-project--directory-subdirs dir)))

(defun vftc-project--subdirs-completion-table (dir)
  "Return list of subdirectories in DIR with completion table."
  (vftc-common-completion-table
   'file
   (vftc-project--directory-subdirs-no-git dir)))

(defvar vftc-project--subdir-hist '()
  "Minibuffer history for `vftc-project-find-subdir'.")

(defun vftc-project--project-current ()
  "Return directory from `project-current' based on Emacs version."
  (if (>= emacs-major-version 29)
      (project-root (project-current))
    (cdr (project-current))))

;;;###autoload
(defun vftc-project-find-subdir ()
  "Find subdirectories in the current project, using completion."
  (interactive)
  (let* ((dir (vftc-project--project-current))
         (subdirs (vftc-project--subdirs-completion-table dir))
         (directory (completing-read "Select Project subdir: " subdirs
                                     nil t nil 'vftc-project--subdir-hist)))
    (dired directory)
    (add-to-history 'vftc-project--subdir-hist dir)))

;;;###autoload
(defun vftc-project-commit-log (&optional arg)
  "Print commit log for the current project.
With optional prefix ARG (\\[universal-argument]) shows expanded
commit messages and corresponding diffs.

The log is limited to the integer specified by
`vftc-project-commit-log-limit'.  A value of 0 means
'unlimited'."
  (interactive "P")
  (let* ((dir (vftc-project--project-current))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (backend (vc-responsible-backend dir))
         (num vftc-project-commit-log-limit)
         (int (vftc-common-number-integer-p num))
         (limit (if (= int 0) t int))
         (diffs (if arg 'with-diff nil))
         (vc-log-short-style (unless diffs '(directory))))
    (vc-print-log-internal backend (list dir) nil nil limit diffs)))

;;;###autoload
(defun vftc-project-retrieve-tag ()
  "Run `vc-retrieve-tag' on project and switch to the root dir.
Basically switches to a new branch or tag."
  (interactive)
  (let* ((dir (vftc-project--project-current))
         (default-directory dir) ; otherwise fails at spontaneous M-x calls
         (name
          (vc-read-revision "Tag name: "
                            (list dir)
                            (vc-responsible-backend dir))))
    (vc-retrieve-tag dir name)
    (project-dired)))

(autoload 'magit-status "magit")

;;;###autoload
(defun vftc-project-magit-status ()
  "Run `magit-status' on project."
  (interactive)
  (magit-status (vftc-project--project-current)))

(defun vftc-project--max-line ()
  "Return the last line's number."
  (save-excursion
    (goto-char (point-max))
    (line-number-at-pos)))

(defun vftc-project--large-file-p (&optional n)
  "Check if lines exceed `vftc-project-large-file-lines'.
Optional N integer overrides that variable's value."
  (let* ((num (or n vftc-project-large-file-lines))
         (int (vftc-common-number-integer-p num)))
    (> (vftc-project--max-line) int)))

;;;###autoload
(defun vftc-project-flymake-mode-activate ()
  "Activate Flymake only for `project-known-project-roots'."
  (project--ensure-read-project-list)
  (let ((known-projects (project-known-project-roots))
        (pr (or (vc-root-dir)
                (locate-dominating-file "." ".git")
                default-directory))
        (modes (vftc-common-minor-modes-active)))
    (if (and (null buffer-read-only)
             (member pr known-projects)
             (not (vftc-project--large-file-p))
             (not (member 'org-src-mode modes))
             (not (null buffer-file-truename)))
        (flymake-mode 1)
      (flymake-mode -1))))

;(defvar org-src-mode-hook)

;(add-hook 'org-src-mode-hook #'vftc-project-flymake-mode-activate)
;(add-hook 'prog-mode-hook #'vftc-project-flymake-mode-activate)

(provide 'vftc-project)

;;; vftc-project.el ends here
