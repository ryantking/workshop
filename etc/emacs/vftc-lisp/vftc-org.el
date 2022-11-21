;;; vftc-org.el --- VFTC Emacs org extensions -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Some configuration and utilities for org.

;;; Code:

(defun vftc-org-save-all ()
  "Saves all open buffers without user confirmation."
  (interactive)
  (message "Saving org-agenda files...")
  (save-some-buffers t (lambda ()
			 (when (member (ff-basename (buffer-file-name)) org-agenda-files) t)))
  (message "Saving org-agenda files...done"))

(defun vftc-org-delegate ()
  "Delegate item at point."
  (interactive)
  (let ((delegated-to (read-string "Delegate to: "))
	(org-inhibit-logging 'note))
    (org-set-property "DELEGATED_TO" delegated-to)
    (org-todo "HOLD")
    (org-schedule 0)
    (save-excursion
      (goto-char (org-log-beginning t))
      (insert (format "delegated to %s\n" delegated-to)))))

;;
;;; org-capture

(defun vftc-org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(declare-function cl-letf "cl-lib")

(defun vftc-org--capture-no-delete-windows (oldfun &rest args)
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

(advice-add 'org-capture-place-template :around 'vftc-org--capture-no-delete-windows)

;;
;;; org-agenda

(declare-function calendar-day-name "calendar")
(declare-function calendar-day-of-week "calendar")
(declare-function calendar-month-name "calendar")
(declare-function org-days-to-iso-week "org")
(declare-function calendar-absolute-from-gregorian "calendar")

(defvar org-agenda-format-date)

;;;###autoload
(defun vftc-org-agenda-format-date-aligned (date)
  (require 'cal-iso)
  (let* ((dayname (calendar-day-name date t))
	 (day (cadr date))
	 (day-of-week (calendar-day-of-week date))
	 (month (car date))
	 (monthname (calendar-month-name month t))
	 (year (nth 2 date))
	 (iso-week (org-days-to-iso-week (calendar-absolute-from-gregorian date)))
	 (weekstring (if (= day-of-week 1)
			 (format " (W%02d)" iso-week)
		       "")))
    (format "%s %2d %s %4d%s" dayname day monthname year weekstring)))

(defvar org-priority-highest)

(defun vftc-org-agenda-delegate ()
  "Delegate the current agenda task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'vftc-org-agenda-delegate nil t nil
   (let* ((marker (or (org-get-at-bol 'org-marker) (org-agenda-error)))
	  (buffer (marker-buffer marker))
	  (pos (marker-position marker)))
     (set-marker-insertion-type t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (vftc-org-delegate))
       (org-agenda-show-tags)))))

(defun vftc-org-cancel-project ()
  "Cancel all tasks in the current project"
  (interactive)
  (org-edna-mode -1)
  (org-map-entries
   (lambda ()
     (when (and (org-entry-is-todo-p) (not (org-entry-is-done-p)))
       (let ((org-inhibit-logging 'note))
	 (org-todo "CNCL")))) nil 'tree)
  (org-edna-mode 1))

;;
;;; org-present

(defun vftc-org-present--prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun vftc-org-present-hook ()
  (setq-local face-remapping-alist '((default (:height 1.5) variable-pitch)
                                     (header-line (:height 4.5) variable-pitch)
                                     (org-document-title (:height 1.75) org-document-title)
                                     (org-code (:height 1.55) org-code)
                                     (org-verbatim (:height 1.55) org-verbatim)
                                     (org-block (:height 1.25) org-block)
                                     (org-block-begin-line (:height 0.7) org-block)))
  (setq header-line-format " ")
  (org-appear-mode -1)
  (org-display-inline-images)
  (vftc-org-present--prepare-slide))

(defun vftc-org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-appear-mode 1))

(defun vftc-org-present-prev ()
  (interactive)
  (org-present-prev)
  (vftc-org-present--prepare-slide))

(defun vftc-org-present-next ()
  (interactive)
  (org-present-next)
  (vftc-org-present--prepare-slide)
  (when (fboundp 'live-crafter-add-timestamp)
    (live-crafter-add-timestamp (substring-no-properties (org-get-heading t t t t)))))

(provide 'vftc-org)

;;; vftc-org.el ends here
