;;; vftc-go.el --- VFTC Emacs go extensions -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Some doom emacs go functionality that I gone and took.

;;; Code:

(defvar vftc-go--last-test nil
  "The last go test that was run.")

(defun vftc-go--run (cmd)
  (save-selected-window
    (compile cmd)))

(defun vftc-go--run-tests (args)
  (let ((cmd (concat "go test -test.v " args)))
    (setq vftc-go--last-test (concat "cd " default-directory ";" cmd))
    (vftc-go--run cmd)))

;;;###autoload
(defun vftc-go-test-rerun ()
  (interactive)
  (if vftc-go--last-test
      (vftc-go--run vftc-go--last-test)
    (vftc-go-test-all)))

;;;###autoload
(defun vftc-go-test-all ()
  (interactive)
  (vftc-go--run-tests ""))

;;;###autoload
(defun vftc-go-test-nested ()
  (interactive)
  (vftc-go--run-tests "./..."))

;;;###autoload
(defun vftc-go-test-single ()
  (interactive)
    (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (re-search-backward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)")
        (vftc-go--run-tests (concat "-run" "='^\\Q" (match-string-no-properties 2) "\\E$'")))
      (error "Must be in a _test.go file")))

;;;###autoload
(defun vftc-go-test-file ()
  (interactive)
  (if (string-match "_test\\.go" buffer-file-name)
      (save-excursion
        (goto-char (point-min))
        (let ((func-list))
          (while (re-search-forward "^func[ ]+\\(([[:alnum:]]*?[ ]?[*]?[[:alnum:]]+)[ ]+\\)?\\(Test[[:alnum:]_]+\\)(.*)" nil t)
            (push (match-string-no-properties 2) func-list))
          (+go--run-tests (concat "-run" "='^(" (string-join func-list "|")  ")$'"))))
    (error "Must be in a _test.go file")))

(provide 'vftc-go)

;;; vftc-go.el ends here
