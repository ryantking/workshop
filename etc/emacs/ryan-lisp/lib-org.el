;;; lib-org.el --- Org mode extensions -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan <ryan@carelesslisper.xyz>

;; Author: Ryan <ryan@carelesslisper.xyz>
;; URL: https://github.com/ryantking/system
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some configuration and utilities for org.

;;; Code:

(ryan-emacs-elpa-package 'noflet)

(defun ryan-org-save-all ()
  "Saves all open buffers without user confirmation."
  (interactive)
  (message "Saving org-agenda files...")
  (save-some-buffers t (lambda ()
			 (when (member (ff-basename (buffer-file-name)) org-agenda-files) t)))
  (message "Saving org-agenda files...done"))

(defun ryan-org-delegate ()
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

;;;; org-capture

(defun ryan-org-capture-inbox ()
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "i"))

(defun ryan-org-capture-full-frame ()
  (interactive)
  (let ((display-buffer-alist `((".*" (display-buffer-full-frame)))))
    (org-capture nil "i")))

(declare-function cl-letf "cl-lib")

(defun ryan-org--capture-no-delete-windows (oldfun &rest args)
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore))
    (apply oldfun args)))

(advice-add 'org-capture-place-template :around 'ryan-org--capture-no-delete-windows)

;;;###autoload
(defvar ryan-org-capture--frame-parameters
  `((name . "org-capture")
    (width . 70)
    (height . 25)
    (transient . t)))

;;;###autoload
(defun ryan-org-capture-cleanup-frame-h ()
  (when (and (ryan-org-capture--frame-p)
             (not org-capture-is-refiling))
    (delete-frame nil t)))

;;;###autoload
(defun ryan-org-capture--frame-p (&rest _)
  (and (equal (alist-get 'name ryan-org-capture--frame-parameters)
              (frame-parameter nil 'name))
       (frame-parameter nil 'transient)))

;;;###autoload
(defun ryan-org-capture-open-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (delete-other-windows)
  (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture nil "i")))

;;;; org-agenda

(declare-function calendar-day-name "calendar")
(declare-function calendar-day-of-week "calendar")
(declare-function calendar-month-name "calendar")
(declare-function org-days-to-iso-week "org")
(declare-function calendar-absolute-from-gregorian "calendar")

(defvar org-agenda-format-date)

;;;###autoload
(defun ryan-org-agenda-format-date-aligned (date)
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

(defun ryan-org-agenda-delegate ()
  "Delegate the current agenda task."
  (interactive)
  (org-agenda-check-type t 'agenda 'todo 'tags 'search)
  (org-agenda-check-no-diary)
  (org-agenda-maybe-loop
   #'ryan-org-agenda-delegate nil t nil
   (let* ((marker (or (org-get-at-bol 'org-marker) (org-agenda-error)))
	  (buffer (marker-buffer marker))
	  (pos (marker-position marker)))
     (set-marker-insertion-type t)
     (org-with-remote-undo buffer
       (with-current-buffer buffer
	 (widen)
	 (goto-char pos)
	 (ryan-org-delegate))
       (org-agenda-show-tags)))))

(defun ryan-org-cancel-project ()
  "Cancel all tasks in the current project"
  (interactive)
  (org-edna-mode -1)
  (org-map-entries
   (lambda ()
     (when (and (org-entry-is-todo-p) (not (org-entry-is-done-p)))
       (let ((org-inhibit-logging 'note))
	 (org-todo "CNCL")))) nil 'tree)
  (org-edna-mode 1))

;;;; org-present

(defun ryan-org-present--prepare-slide ()
  (org-overview)
  (org-show-entry)
  (org-show-children))

(defun ryan-org-present-hook ()
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
  (ryan-org-present--prepare-slide))

(defun ryan-org-present-quit-hook ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (setq header-line-format nil)
  (org-present-small)
  (org-remove-inline-images)
  (org-appear-mode 1))

(defun ryan-org-present-prev ()
  (interactive)
  (org-present-prev)
  (ryan-org-present--prepare-slide))

(defun ryan-org-present-next ()
  (interactive)
  (org-present-next)
  (ryan-org-present--prepare-slide)
  (when (fboundp 'live-crafter-add-timestamp)
    (live-crafter-add-timestamp (substring-no-properties (org-get-heading t t t t)))))

(provide 'lib-org)

;;; lib-org.el ends here
