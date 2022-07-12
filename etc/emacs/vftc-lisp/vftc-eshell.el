;;; vftc-vc.el --- VFTC Emacs vc.el Functions -*- lexical-binding: t -*-

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

;; Some utility functions that augment/enhance builtin VC cmomands.

;;; Code:

(require 'vc)
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
  (let ((current-branch (car (vc-git-branches))))
    (concat
     "\n"
     (propertize (vftc-eshell--prompt-path) 'face `(:foreground ,(face-foreground 'vftc-face-blue) :weight bold))
     (when current-branch (propertize (concat " " current-branch) 'face 'vftc-face-base-4))
     (when (length> (magit-changed-files "HEAD") 0) (propertize "*" 'face 'vftc-face-magenta))
     "\n"
     (if (= (user-uid) 0) (propertize "#" 'face 'vftc-face-red) (propertize "λ" 'face 'vftc-face-green))
     (propertize " " 'face 'vftc-face-fg))))

(provide 'vftc-eshell)

;;; vftc-eshell.el ends here
