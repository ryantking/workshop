;;; ryan-common.el --- Common functions -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan <ryan@carelesslisper.xyz>

;; Author: Ryan <ryan@carelesslisper.xyz>
;; URL: https://github.com/ryantking/system
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Common utility functions.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defgroup ryan-common()
  "Auxiliary functions for RYAN Emacs."
  :group 'editing)

;;;###autoload
(defun ryan-common-number-even-p (n)
  "Test if N is an even number."
  (if (numberp n)
      (= (% n 2) 0)
    (error "%s is not a number" n)))

;;;###autoload
(defun ryan-common-number-integer-p (n)
  "Test if N is an integer."
  (if (integerp n)
      n
    (error "%s is not an integer" n)))

;;;###autoload
(defun ryan-common-number-integer-positive-p (n)
  "Test if N is a positive integer."
  (if (ryan-common-number-integer-p n)
      (> n 0)
    (error "%s is not a positive integer" n)))

;; Thanks to Gabriel for providing a cleaner version of
;; `ryan-common-number-negative': <https://github.com/gabriel376>.
;;;###autoload
(defun ryan-common-number-negative (n)
  "Make N negative."
  (if (and (numberp n) (> n 0))
      (* -1 n)
    (error "%s is not a valid positive number" n)))

(defun ryan-common-completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

;;;###autoload
(defun ryan-common-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              v      (push m active-modes)))
          minor-mode-list)
    active-modes))

(defvar ryan-common-url-regexp
  (concat
   "\\b\\(\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|"
   "nntp\\|news\\|telnet\\|wais\\|mailto\\|info\\):\\)"
   "\\(//[-a-z0-9_.]+:[0-9]*\\)?"
   (let ((chars "-a-z0-9_=#$@~%&*+\\/[:word:]")
	 (punct "!?:;.,"))
     (concat
      "\\(?:"
      ;; Match paired parentheses, e.g. in Wikipedia URLs:
      ;; http://thread.gmane.org/47B4E3B2.3050402@gmail.com
      "[" chars punct "]+" "(" "[" chars punct "]+" ")"
      "\\(?:" "[" chars punct "]+" "[" chars "]" "\\)?"
      "\\|"
      "[" chars punct "]+" "[" chars "]"
      "\\)"))
   "\\)")
  "Regular expression that matches URLs.
Copy of variable `browse-url-button-regexp'.")

;;;###autoload
(defun ryan-common-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

(provide 'lib-common)

;;; lib-common.el ends here
