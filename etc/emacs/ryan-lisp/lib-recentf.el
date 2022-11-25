;;; lib-recentf.el --- Extensions to recentf.el -*- lexical-binding: t -*-

;; Author: Ryan <ryan@carelesslisper.xyz>
;; URL: https://github.com/ryantking/system
;; Version: 0.3.0

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

;; Exetensions to rexentf.

;;; Code:

(require 'recentf)
(require 'lib-common)

    ;;;###autoload
(defun ryan-recentf-keep-predicate (file)
  "Additional conditions for saving FILE in `recentf-list'.
Add this function to `recentf-keep'."
  (cond
   ((file-directory-p file) (file-readable-p file))))

(defvar ryan-recentf--history-files '()
  "Minibuffer history for ryan-recentf files.")

(defvar ryan-recentf--history-dirs '()
  "Minibuffer history for ryan-recentf directories.")

(defun ryan-recentf--files ()
  "Return completion table with files in `recentf-list'."
  (ryan-common-completion-table
   'file
   (mapcar 'abbreviate-file-name recentf-list)))

(defun ryan-recentf--files-prompt (files)
  "Helper of `ryan-recentf-recent-files' to read FILES."
  (let ((def (car ryan-recentf--history-files)))
    (completing-read
     (format "Recentf [%s]: " def)
     files nil t nil 'ryan-recentf--history-files def)))

;;;###autoload
(defun ryan-recentf-recent-files (file)
  "Select FILE from `recentf-list' using completion."
  (interactive
   (list (ryan-recentf--files-prompt (ryan-recentf--files))))
  (find-file file)
  (add-to-history 'ryan-recentf--history-files file))

(defun ryan-recentf--dirs ()
  "Return completion table with directories in `recentf-list'."
  (let ((list (mapcar 'abbreviate-file-name recentf-list)))
    (ryan-common-completion-table
     'file
     (delete-dups
      (mapcar (lambda (file)
                (if (file-directory-p file)
                    (directory-file-name file)
                  (substring (file-name-directory file) 0 -1)))
              list)))))

(defun ryan-recentf--dirs-prompt (dirs)
  "Helper of `ryan-recentf-recent-dirs' to read DIRS."
  (let ((def (car ryan-recentf--history-dirs)))
    (completing-read
     (format "Recent dir [%s]: " def)
     dirs nil t nil 'ryan-recentf--history-dirs def)))

;;;###autoload
(defun ryan-recentf-recent-dirs (dir)
  "Select DIR from `recentf-list' using completion."
  (interactive
   (list (ryan-recentf--dirs-prompt (ryan-recentf--dirs))))
  (find-file dir)
  (add-to-history 'ryan-recentf--history-dirs dir))

;;;###autoload
(defun ryan-recentf-recent-files-or-dirs (&optional arg)
  "Select recent file or, with ARG, recent directory."
  (interactive "P")
  (if arg
      (call-interactively 'ryan-recentf-recent-dirs)
    (call-interactively 'ryan-recentf-recent-files)))

(provide 'lib-recentf)

;;; lib-recentf.el ends here
