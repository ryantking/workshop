;;; vftc-mail.el --- VFTC Emacs mail extensions -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@protonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Add unread mail to the modeline.

;;; Code:

(require 'vftc-common)

(defgroup vftc-mail ()
  "Extensions for mail."
  :group 'mail)

(defcustom vftc-mail-maildir-path-regexp "~/.mail/*/Inbox/new/"
  "Path used to find new mail."
  :type 'string
  :group 'vftc-mail)

(defcustom vftc-mail-mode-line-indicator-commands '(notmuch-refresh-this-buffer)
  "List of command sthat will be advised to update the mode line."
  :type 'list
  :group 'vftc-mail)

(defvar ebdb-db-list)
(autoload 'ebdb-load "ebdb")

(when (require 'ebdb nil t)
  (defun vftc-mail-ebdb-message-startup ()
    "Load EBDB if not done already."
    (unless ebdb-db-list
      (ebdb-load))))

(defface vftc-mail-count
  '((t
     :weight bold
     :inherit vftc-face-magenta))
  "Face for the modeline indicator")

(defvar vftc-mail-new-mail-string nil
  "New maildir count number.")

(defun vftc-mail--new-mail ()
  "Search for new mail in the maildir paths."
  (with-temp-buffer
    (shell-command
     (format "fd . %s -t f | wc -l" vftc-mail-maildir-path-regexp) t)
    (buffer-substring-no-properties (point-min) (1- (point-max)))))

(defun vftc-mail--mode-string (count)
  "Add properties to COUNT string."
  (when (not (string= count "0")
	     (propertize (format "@%s " count)
			 'face 'vftc-mail-count
			 'help-echo "Number of new items in the maildir"))))

(defvar vftc-mail--mode-line-indicator nil
  "Internal variable used to store the state of new mail.")

(defun vftc-mail--mode-line-indicator ()
  "Prepare the new mail count indicator."
  (let* ((count (vftc-mail--new-mail))
	 (indicator (vftc-mail--mode-string count))
	 (old-indicator vftc-mail--mode-line-indicator))
    (when old-indicator
      (setq global-mode-string (delete old-indicator global-mode-string)))
    (cond
     ((>= (string-to-number count) 1)
      (setq global-mode-string (push indicator global-mode-string)
	    vftc-mail--mode-line-indicator indicator))
     (t
      (setq vftc-mail--mode-line-indicator nil)))))

(defvar vftc-mail--mode-line-sync-hook nil
  "Hook to refresh the mode line indicator.")

(defun vftc-mail--add-hook (&rest _)
  "Run `vftc-mail--mode-line-sync-hook'."
  (run-hooks 'vftc-mail--mode-line-sync-hook))

;;;###autoload
(define-minor-mode vftc-mail-indicator
  "Enable mode line indicator with mail count."
  :init-value nil
  :global t
  (if vftc-mail-indicator
      (progn
	(run-at-time t 60 #'vftc-mail--mode-line-indicator)
	(when vftc-mail-mode-line-indicator-commands
	  (dolist (fn vftc-mail-mode-line-indicator-commands)
	    (advice-add fn :after #'vftc-mail--add-hook)))
	(add-hook 'vftc-mail--mode-line-sync-hook #'vftc-mail--mode-line-indicator)
	(force-mode-line-update t))
    (cancel-function-timers #'vftc-mail--mode-line-indicator)
    (setq global-mode-string (delete vftc-mail--mode-line-indicator global-mode-string))
    (remove-hook 'vftc-mail--mode-line-sync-hook #'vftc-mail--mode-line-indicator)
    (when vftc-mail-mode-line-indicator-commands
      (dolist (fn vftc-mail-mode-line-indicator-commands)
	(advice-remove fn #'vftc-mail--add-hook)))
    (force-mode-line-update t)))

(provide 'vftc-mail)

;;; vftc-mail.el ends here
