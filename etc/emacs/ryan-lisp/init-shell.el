;;; init-shell.el --- Shell configuration -*- lexical-binding: t -*-

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

;; Configure the emacs shell and other shells.

;;; Code:

;;; The emacs shell (eshell.el)
(ryan-emacs-builtin-package 'eshell
  (require 'esh-mode)
  (require 'esh-module)

  (setq eshell-history-size 10000
	eshell-buffer-maximum-lines 10000
	eshell-scroll-to-bottom-on-input t)

  (setenv "PAGER" "cat")

  (push 'eshell-tramp eshell-modules-list)
  (add-hook 'eshell-pre-command-hook #'eshell-save-some-history)
  (define-key eshell-mode-map (kbd "C-r") #'consult-history)

  (require 'em-cmpl)
  (require 'em-dirs)
  (require 'em-tramp)
  (require 'em-hist)

  (setq password-cache t
	password-cache-expiry 600
	eshell-hist-ignoredups t))

;;; Eshell prompt (lib-eshell.el)
(ryan-emacs-builtin-package 'lib-eshell
  (setq eshell-prompt-function #'ryan-eshell-prompt
	eshell-prompt-regexp "^λ "))

;;; Unix shell (shell.el)
(ryan-emacs-builtin-package 'shell
  (setq shell-command-prompt-show-cwd t))

;;; Enhanced color support (xterm-color.el)
(ryan-emacs-elpa-package 'xterm-color
    ;;; Eshell
  (add-hook 'eshell-before-prompt-hook
	    (lambda ()
	      (setq xterm-color-preserve-properties t)))

  (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
  (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

  ;; Only enable xterm colors during commands
  (add-hook 'eshell-pre-command-hook (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook (lambda () (setenv "TERM" "dumb")))

    ;;; Compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (advice-add
   'compilation-filter
   :around
   (lambda (f proc string)
     (funcall f proc (xterm-color-filter string)))))

;;; Process monitor (proced.el)
(ryan-emacs-builtin-package 'proced
  (setq proced-auto-update-flag t))

;;; Password store (password-store.el)
(ryan-emacs-elpa-package 'password-store
  (setq password-store-time-before-clipboard-restore 30)
  (define-key global-map (kbd "C-c k") #'password-store-copy))

;;; Password store major mode (pass.el)
(ryan-emacs-elpa-package 'pass)

(provide 'init-shell)

;;; init-shell.el ends here
