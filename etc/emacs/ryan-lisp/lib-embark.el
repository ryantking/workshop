;;; lib-embark.el --- Embark Extensions -*- lexical-binding: t -*-

;; Author: Ryan <ryan@carelesslisper.xyz>
;; URL: https://github.com/ryantking/system
;; Version: 0.3.0

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

;; Extensions to the embark backage.

;;; Code:

(require 'cl-lib)
(require 'embark nil t)
(require 'lib-recentf)

(defgroup ryan-embark ()
  "Extensions for `embark'."
  :group 'editing)

;;
;;; Extra keymaps

(autoload 'consult-ripgrep "counsult")
(autoload 'consult-line "consult")
(autoload 'consult-imenu "consult")
(autoload 'consult-outline "consult")

(defvar ryan-embark-become-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'consult-find)
    (define-key map (kbd "g") 'consult-ripgrep)
    map)
  "General custom cross-package `embark-become' keymap.")

(defvar ryan-embark-become-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "s") 'consult-outline)
    map)
  "Line-specific custom cross-package `embark-become' keymap.")

(defvar embark-become-file+buffer-map)
(autoload 'ryan-recentf-recent-files "ryan-recentf")
(autoload 'project-switch-to-buffer "project")
(autoload 'project-find-file "project")

(defvar ryan-embark-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    (define-key map (kbd "r") 'ryan-recentf-recent-files)
    (define-key map (kbd "B") 'project-switch-to-buffer)
    (define-key map (kbd "F") 'project-find-file)
    map)
  "File+buffer custom cross-package `embark-become' keymap.")

(defvar embark-become-keymaps)

;;;###autoload
(define-minor-mode ryan-embark-keymaps
  "Add or remove keymaps from Embark.
This is based on the value of `ryan-embark-add-keymaps'
and is meant to keep things clean in case I ever wish to disable
those so-called 'extras'."
  :init-value nil
  :global t
  (let ((maps (list 'ryan-embark-become-general-map
                    'ryan-embark-become-line-map
                    'ryan-embark-become-file+buffer-map)))
    (if ryan-embark-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))

(provide 'lib-embark)

;;; lib-embark.el ends here
