;;; lib-sideline.el --- Emacs sideeline -*- lexical-binding: t -*-

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

;; Shows line numbers and other information on the sidebar.

;;; Code:

(defgroup ryan-sideline ()
  "Setup for editing text-heavy buffers."
  :group 'files)

(define-minor-mode ryan-sideline-mode
  "Buffer-local wrapper mode for presentations."
  :init-value nil
  :global ni)

(autoload 'diff-hl-mode "diff-hl")

(defun ryan-sideline--diff-hl-toggle ()
  "Toggle buffer local diff indicators in the fringe"
  (if (or (bound-and-true-p diff-hl-mode)
	      (not (bound-and-true-p ryan-sideline-mode)))
      (diff-hl-mode -1)
    (diff-hl-mode 1)))

(add-hook 'ryan-sideline-mode-hook #'ryan-sideline--diff-hl-toggle)

(defun ryan-sideline--numbers-toggle ()
  "Toggle line numbers."
  (if (or (bound-and-true-p display-line-numbers-mode)
          (not (bound-and-true-p ryan-sideline-mode)))
      (display-line-numbers-mode -1)
    (display-line-numbers-mode 1)))

(add-hook 'ryan-sideline-mode-hook #'ryan-sideline--numbers-toggle)

(defun ryan-sideline--hl-line-toggle ()
  "Toggle line highlight."
  (if (or (bound-and-true-p hl-line-mode)
          (not (bound-and-true-p ryan-sideline-mode)))
      (hl-line-mode -1)
    (hl-line-mode 1)))

(add-hook 'ryan-sideline-mode-hook #'ryan-sideline--hl-line-toggle)

(autoload 'whitespace-mode "whitespace")

;;;###autoload
(defun ryan-sideline-negative-space-toggle ()
  "Toggle the display of indentation and space characters."
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (whitespace-mode -1)
    (whitespace-mode)))

(provide 'lib-sideline)

;;; lib-sideline.el ends here
