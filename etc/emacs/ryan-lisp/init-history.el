;;; init-history.el --- History management -*- lexical-binding: t -*-

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

;; Configures how emacs stores file history.

;;; Code:

;;; Record cursor position
(ryan-emacs-builtin-package 'saveplace
  (save-place-mode 1))

;;;; Backup files

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backup/")))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      create-lockfiles nil)

(let ((auto-save-directory (concat user-emacs-directory "auto-save/")))
  auto-save-file-name-transforms `((".*" ,auto-save-directory t))

  (unless (file-exists-p auto-save-directory)
    (make-directory auto-save-directory)))

(provide 'init-history)

;;; init-history.el ends here
