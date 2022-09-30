;;; vftc-vc.el --- VFTC Emacs vc.el Functions -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; Some utility functions that augment/enhance builtin VC cmomands.

;;; Code:

(require 'magit)

(defun vftc-eshell--prompt-path ()
  (let* ((current-path (eshell/pwd))
	 (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
	 (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
	(abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun vftc-eshell-prompt ()
  "A custom prompt for eshell that more closely replicates a modern prompt such as starship."
  (let ((current-branch (magit-get-current-branch)))
    (concat
     "\n"
     (propertize (vftc-eshell--prompt-path) 'face `(:foreground ,(face-foreground 'vftc-face-blue) :weight bold))
     (when current-branch
       (propertize (concat " " current-branch) 'face 'vftc-face-base-4))
     (when (and current-branch (length> (magit-changed-files "HEAD") 0))
       (propertize "*" 'face 'vftc-face-magenta))
     "\n"
     (if (= (user-uid) 0) (propertize "#" 'face 'vftc-face-red) (propertize "λ" 'face 'vftc-face-green))
     (propertize " " 'face 'vftc-face-fg))))

(provide 'vftc-eshell)

;;; vftc-eshell.el ends here
