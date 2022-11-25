;;; lib-go.el --- Go extensions -*- lexical-binding: t -*-

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

;; Some doom emacs go functionality that I gone and took.

;;; Code:

(defvar ryan-go--last-test nil
  "The last go test that was run.")

(defun ryan-go--run (cmd)
  (save-selected-window
    (compile cmd)))

(defun ryan-go--run-tests (args)
  (let ((cmd (concat "go test -test.v " args)))
    (setq ryan-go--last-test (concat "cd " default-directory ";" cmd))
    (ryan-go--run cmd)))

;;;###autoload
(defun ryan-go-test-rerun ()
  (interactive)
  (if ryan-go--last-test
      (ryan-go--run ryan-go--last-test)
    (ryan-go-test-all)))

;;;###autoload
(defun ryan-go-test-all ()
  (interactive)
  (ryan-go--run-tests ""))

;;;###autoload
(defun ryan-go-test-nested ()
  (interactive)
  (ryan-go--run-tests "./..."))

;;;###autoload
(defun ryan-go-test-single ()
  (interactive)
    (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (ryan-go--run-tests (concat "-run" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
      (error "Must be in a _test.go file")))

;;;###autoload
(defun ryan-go-test-file ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (goto-char (point-min))
        (let ((func-list))
          (while (re-search-forward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)" nil t)
            (push (match-string-no-properties 2) func-list))
          (+go--run-tests (concat "-run" "='^(" (string-join func-list "|")  ")$'"))))
    (error "Must be in a _test.go file")))

(provide 'lib-go)

;;; lib-go.el ends here
