;;; vftc-common.el --- Common functions -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file sets up VFTC emacs with the core package management macros.

;;; Code:

(eval-when-compile
  (require 'subr-x))

(defgroup vftc-common()
  "Auxiliary functions for VFTC Emacs."
  :group 'editing)

;;;###autoload
(defun vftc-common-number-even-p (n)
  "Test if N is an even number."
  (if (numberp n)
      (= (% n 2) 0)
    (error "%s is not a number" n)))

;;;###autoload
(defun vftc-common-number-integer-p (n)
  "Test if N is an integer."
  (if (integerp n)
      n
    (error "%s is not an integer" n)))

;;;###autoload
(defun vftc-common-number-integer-positive-p (n)
  "Test if N is a positive integer."
  (if (vftc-common-number-integer-p n)
      (> n 0)
    (error "%s is not a positive integer" n)))

;; Thanks to Gabriel for providing a cleaner version of
;; `vftc-common-number-negative': <https://github.com/gabriel376>.
;;;###autoload
(defun vftc-common-number-negative (n)
  "Make N negative."
  (if (and (numberp n) (> n 0))
      (* -1 n)
    (error "%s is not a valid positive number" n)))

(defun vftc-common-completion-table (category candidates)
  "Pass appropriate metadata CATEGORY to completion CANDIDATES.

This is intended for bespoke functions that need to pass
completion metadata that can then be parsed by other
tools (e.g. `embark')."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (category . ,category))
      (complete-with-action action candidates string pred))))

;;;###autoload
(defun vftc-common-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              v      (push m active-modes)))
          minor-mode-list)
    active-modes))

(defvar vftc-common-url-regexp
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
(defun vftc-common-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

(provide 'vftc-common)

;;; vftc-common.el ends here
