;;; vftc-recentf.el --- Extensions to recentf.el -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
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

;; Extensions to the `recentf.el' library.

;;; Code:

(require 'recentf)
(require 'vftc-common)

    ;;;###autoload
(defun vftc-recentf-keep-predicate (file)
  "Additional conditions for saving FILE in `recentf-list'.
Add this function to `recentf-keep'."
  (cond
   ((file-directory-p file) (file-readable-p file))))

(defvar vftc-recentf--history-files '()
  "Minibuffer history for vftc-recentf files.")

(defvar vftc-recentf--history-dirs '()
  "Minibuffer history for vftc-recentf directories.")

(defun vftc-recentf--files ()
  "Return completion table with files in `recentf-list'."
  (vftc-common-completion-table
   'file
   (mapcar 'abbreviate-file-name recentf-list)))

(defun vftc-recentf--files-prompt (files)
  "Helper of `vftc-recentf-recent-files' to read FILES."
  (let ((def (car vftc-recentf--history-files)))
    (completing-read
     (format "Recentf [%s]: " def)
     files nil t nil 'vftc-recentf--history-files def)))

;;;###autoload
(defun vftc-recentf-recent-files (file)
  "Select FILE from `recentf-list' using completion."
  (interactive
   (list (vftc-recentf--files-prompt (vftc-recentf--files))))
  (find-file file)
  (add-to-history 'vftc-recentf--history-files file))

(defun vftc-recentf--dirs ()
  "Return completion table with directories in `recentf-list'."
  (let ((list (mapcar 'abbreviate-file-name recentf-list)))
    (vftc-common-completion-table
     'file
     (delete-dups
      (mapcar (lambda (file)
                (if (file-directory-p file)
                    (directory-file-name file)
                  (substring (file-name-directory file) 0 -1)))
              list)))))

(defun vftc-recentf--dirs-prompt (dirs)
  "Helper of `vftc-recentf-recent-dirs' to read DIRS."
  (let ((def (car vftc-recentf--history-dirs)))
    (completing-read
     (format "Recent dir [%s]: " def)
     dirs nil t nil 'vftc-recentf--history-dirs def)))

;;;###autoload
(defun vftc-recentf-recent-dirs (dir)
  "Select DIR from `recentf-list' using completion."
  (interactive
   (list (vftc-recentf--dirs-prompt (vftc-recentf--dirs))))
  (find-file dir)
  (add-to-history 'vftc-recentf--history-dirs dir))

;;;###autoload
(defun vftc-recentf-recent-files-or-dirs (&optional arg)
  "Select recent file or, with ARG, recent directory."
  (interactive "P")
  (if arg
      (call-interactively 'vftc-recentf-recent-dirs)
    (call-interactively 'vftc-recentf-recent-files)))

(provide 'vftc-recentf)

;;; vftc-recentf.el ends here
