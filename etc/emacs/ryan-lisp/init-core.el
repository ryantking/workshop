;;; init-core.el --- Basic settings and functions -*- lexical-binding: t -*-

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

;; General settings.

;;; Code:

;; Common utility functions (lib-common.el)
(ryan-emacs-builtin-package 'lib-common)

;; Fix mac bindings
(setq mac-option-modifier 'meta
      mac-command-modifier 'hyper)

;; Simple auxillary commands (lib-simple.el) 
(ryan-emacs-builtin-package 'lib-simple
  (setq help-window-select t)

  (let ((map global-map))
    (define-key map (kbd "<insert>") nil)
    (define-key map (kbd "C-z") nil)
    (define-key map (kbd "C-x C-z") nil)
    (define-key map (kbd "C-h h") nil)
    (define-key map (kbd "M-`") nil)
    (define-key map (kbd "C-h .") #'ryan-simple-describe-symbol)
    (define-key map (kbd "C-h K") #'describe-keymap)
    (define-key map (kbd "C-h c") #'describe-char)
    (define-key map (kbd "C-S-w") #'ryan-simple-copy-line-or-region)
    (define-key map (kbd "C-S-y") #'ryan-simple-yank-replace-line-or-region)
    (define-key map (kbd "M-SPC") #'cycle-spacing)
    (define-key map (kbd "M-k") #'ryan-simple-kill-line-backward)
    (define-key map (kbd "<C-return>") #'ryan-simple-new-line-below)
    (define-key map (kbd "<C-S-return>") #'ryan-simple-new-line-above)
    (define-key map (kbd "<C-M-backpace>") #'backward-kill-sexp)
    (define-key map (kbd "M-c") #'capitalize-dwim)
    (define-key map (kbd "M-l") #'downcase-dwim)
    (define-key map (kbd "M-u") #'upcase-dwim)
    (define-key map (kbd "M-@") #'ryan-simple-mark-word)
    (define-key map (kbd "M-Q") #'ryan-simple-unfill-region-or-paragraph)
    (define-key map (kbd "<C-f2>") #'ryan-simple-rename-file-and-buffer)
    (define-key map (kbd "C-x K") #'ryan-simple-kill-buffer-current)))

(let ((map ctl-x-x-map))
  (define-key map "e" #'eval-buffer)
  (define-key map "f" #'follow-mode)
  (define-key map "r" #'rename-uniquely))

;;; Set paths from the system
(ryan-emacs-elpa-package 'exec-path-from-shell
  (setq exec-path-from-shell-variables
	'("PATH" "PGPKEYID" "SSH_AUTH_SOCK"))

  (exec-path-from-shell-initialize))

;;; Mouse wheel behaviour
(ryan-emacs-builtin-package 'mouse
  (setopt mouse-wheel-scroll-amount
          '(1
            ((shift) . 5)
            ((meta) . 0.5)
            ((control) . text-scale)))
  (add-hook 'after-init-hook #'mouse-wheel-mode)
  (define-key global-map (kbd "C-M-<mouse-3>") #'tear-off-window))

;;; Delete selection
(ryan-emacs-builtin-package 'delsel
  (add-hook 'after-init-hook #'delete-selection-mode))

;;; Tooltips (tooltip.el)
(ryan-emacs-builtin-package 'tooltip
  (setq tooltip-delay 0.5
	tooltip-frame-parameters
	'((name . "tooltip")
	  (internal-border-width . 6)
	  (border-width . 0)
	  (no-special-glyphs . t)))
  (add-hook 'after-init-hook #'tooltip-mode))

;;; Auto revert mode
(ryan-emacs-builtin-package 'autorevert
  (add-hook 'after-init-hook #'global-auto-revert-mode))

;;; Preserve system clipboard
(setq save-interprogram-paste-before-kill t)

;;; Newline characters for file ending
(setopt mode-require-final-newline 'visit-save)

;;; Go to last change (goto-last-change.el)
(ryan-emacs-elpa-package 'goto-last-change
  (define-key global-map (kbd "C-z") #'goto-last-change))

;;; Repeatable key chords (reapeat-mode.el)
(ryan-emacs-builtin-package 'repeat
  (setopt repeat-exit-timeout 5
	  repeat-exit-key "<escape>")
  (add-hook 'after-init-hook #'repeat-mode))

;;; Move customization file (cus-edit.el)
(ryan-emacs-builtin-package 'cus-edit
  (setq custom-file (make-temp-file "emacs-custom-")))

;;; Keybinding cheatcheet (which-key.el)
(ryan-emacs-elpa-package 'which-key
  (which-key-mode 1))

(provide 'init-core)

;;; init-core.el ends here
