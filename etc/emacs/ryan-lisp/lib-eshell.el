;;; lib-eshell.el --- Eshell prompt -*- lexical-binding: t -*-

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

;; A nice lookin eshell prompt.

;;; Code:

(require 'magit)

(defun ryan-eshell--prompt-path ()
  (let* ((current-path (eshell/pwd))
	 (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
	 (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
	(abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

(defun ryan-eshell-prompt ()
  "A custom prompt for eshell that more closely replicates a modern prompt such as starship."
  (concat
     "\n"
     (propertize (ryan-eshell--prompt-path) 'face `(:foreground ,(doom-color 'yellow) :weight bold))
     (if (= (user-uid) 0)
	 (propertize " #" 'face `(:foreground ,(doom-color 'red)))
       (propertize " λ" 'face `(:foreground ,(doom-color 'green))))
     (propertize " " 'face `(:foreground ,(doom-color 'fg)))))

(provide 'lib-eshell)

;;; lib-eshell.el ends here
