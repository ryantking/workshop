;;; vftc-embark.el --- VFTC Emacs Embark Config -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This is just an adapation of Prot's sideline mode for managing line
;; numbers and highlighting.

;;; Code:

(require 'cl-lib)
(require 'embark nil t)
(require 'vftc-recentf)

(defgroup vftc-embark ()
  "Extensions for `embark'."
  :group 'editing)

;;
;;; Extra keymaps

(autoload 'consult-ripgrep "counsult")
(autoload 'consult-line "consult")
(autoload 'consult-imenu "consult")
(autoload 'consult-outline "consult")

(defvar vftc-embark-become-general-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'consult-find)
    (define-key map (kbd "g") 'consult-ripgrep)
    map)
  "General custom cross-package `embark-become' keymap.")

(defvar vftc-embark-become-line-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") 'consult-line)
    (define-key map (kbd "i") 'consult-imenu)
    (define-key map (kbd "s") 'consult-outline)
    map)
  "Line-specific custom cross-package `embark-become' keymap.")

(defvar embark-become-file+buffer-map)
(autoload 'vftc-recentf-recent-files "vftc-recentf")
(autoload 'project-switch-to-buffer "project")
(autoload 'project-find-file "project")

(defvar vftc-embark-become-file+buffer-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-become-file+buffer-map)
    (define-key map (kbd "r") 'vftc-recentf-recent-files)
    (define-key map (kbd "B") 'project-switch-to-buffer)
    (define-key map (kbd "F") 'project-find-file)
    map)
  "File+buffer custom cross-package `embark-become' keymap.")

(defvar embark-become-keymaps)

;;;###autoload
(define-minor-mode vftc-embark-keymaps
  "Add or remove keymaps from Embark.
This is based on the value of `vftc-embark-add-keymaps'
and is meant to keep things clean in case I ever wish to disable
those so-called 'extras'."
  :init-value nil
  :global t
  (let ((maps (list 'vftc-embark-become-general-map
                    'vftc-embark-become-line-map
                    'vftc-embark-become-file+buffer-map)))
    (if vftc-embark-keymaps
        (dolist (map maps)
          (cl-pushnew map embark-become-keymaps))
      (setq embark-become-keymaps
            (dolist (map maps)
              (delete map embark-become-keymaps))))))

(provide 'vftc-embark)

;;; vftc-embark.el ends here
