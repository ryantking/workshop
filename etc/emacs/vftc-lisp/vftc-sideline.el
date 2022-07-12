;;; vftc-sideline.el --- VFTC Emacs Sideline -*- lexical-binding: t -*-

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

;; This is just an adapation of Prot's sideline mode for managing line
;; numbers and highlighting.

;;; Code:

(defgroup vftc-sideline ()
  "Faces for Very Fun to Configure Emacs."
  :group 'files)

(define-minor-mode vftc-sideline-mode
  "Buffer-local wrapper mode for presentations."
  :init-value nil
  :global ni)

(autoload 'diff-hl-mode "diff-hl")

(defun vftc-sideline--diff-hl-toggle ()
  "Toggle buffer local diff indicators in the fringe"
  (if (or (bound-and-true-p diff-hl-mode)
	      (not (bound-and-true-p vftc-sideline-mode)))
      (diff-hl-mode -1)
    (diff-hl-mode 1)))

(add-hook 'vftc-sideline-mode-hook #'vftc-sideline--diff-hl-toggle)

(defun vftc-sideline--numbers-toggle ()
  "Toggle line numbers."
  (if (or (bound-and-true-p display-line-numbers-mode)
          (not (bound-and-true-p vftc-sideline-mode)))
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(add-hook 'vftc-sideline-mode-hook #'vftc-sideline--numbers-toggle)

(defun vftc-sideline--hl-line-toggle ()
  "Toggle line highlight."
  (if (or (bound-and-true-p hl-line-mode)
          (not (bound-and-true-p vftc-sideline-mode)))
      (hl-line-mode -1)
    (hl-line-mode 1)))

(add-hook 'vftc-sideline-mode-hook #'vftc-sideline--hl-line-toggle)

(autoload 'whitespace-mode "whitespace")

;;;###autoload
(defun vftc-sideline-negative-space-toggle ()
  "Toggle the display of indentation and space characters."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))

(provide 'vftc-sideline)

;;; vftc-theme.el ends here
