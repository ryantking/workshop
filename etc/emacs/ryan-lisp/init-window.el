;;; init-window.el --- Window configuration -*- lexical-binding: t -*-

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

;; Sets up the behaviour and management of windows.

;;; Code:

;;; Unique names for buffers
(ryan-emacs-builtin-package 'uniquify
  (setq uniquify-buffer-name-style 'forward))

;;; Window rules (window.el)
(ryan-emacs-builtin-package 'window
  (setq display-buffer-alist
	`(;; no window
	  ("\\`\\*Async Shell Command\\*\\'"
	   (display-buffer-no-window))
	  ;; top side window
	  ("\\*\\(Flymake diagnostics\\|Package-Lint\\).*"
	   (display-buffer-in-side-window)
	   (window-height . 0.16)
	   (side . top)
	   (slot . 0))
	  ("\\*Messages.*"
	   (display-buffer-in-side-window)
	   (window-height . 0.16)
	   (side . top)
	   (slot . 1))
	  ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|Flymake log\\)\\*"
	   (display-buffer-in-side-window)
	   (window-height . 0.16)
	   (side . top)
	   (slot . 2))
	  ;; left side window
	  ("\\*\\(.* # Help.*\\|Help\\)\\*"
	   (display-buffer-reuse-mode-window display-buffer-in-side-window)
	   (window-width 0.25)
	   (side . left)
	   (slot . 0))
	  ;; bottom side window
	  ("\\*Org Select\\*"
	   (display-buffer-in-side-window)
	   (dedicated . t)
	   (side . bottom)
	   (slot . 0)
	   (window-parameters . ((mode-line-format . none))))
	  ;; bottom buffer
	  ("\\*Embark Actions\\*"
	   (display-buffer-reuse-mode-window display-buffer-at-bottom)
	   (window-height . fit-window-to-buffer)
	   (window-parameters . ((no-other-window . t)
				 (mode-line-format . none))))
	  ;; below current window
	  ;; ("\\*.*\\(e?shell\\|v?term\\).*"
	  ;;  (display-buffer-reuse-mode-window display-buffer-below-selected)
	  ;;  (window-height . 0.2))
	  ("\\*\\(Calendar\\|Bookmark Annotation\\).*"
	   (display-buffer-reuse-mode-window display-buffer-below-selected)
	   (window-height . fit-window-to-buffer)))
	window-combination-resize t
	even-window-sizes 'height-only
	switch-to-buffer-in-dedicated-window 'pop)

  (add-hook 'help-mode-hook #'visual-line-mode)
  (add-hook 'custom-mode-hook #'visual-line-mode)

  (let ((map global-map))
    (define-key map (kbd "C-x !") #'delete-other-windows-vertically)
    (define-key map (kbd "C-x -") #'fit-window-to-buffer)))

;;; Directional window motions (windmove.el)
(ryan-emacs-builtin-package 'windmove
  (let ((map global-map))
    (define-key map (kbd "C-M-<up>") #'windmove-up)
    (define-key map (kbd "C-M-<right>") #'windmove-right)
    (define-key map (kbd "C-M-<down>") #'windmove-down)
    (define-key map (kbd "C-M-<left>") #'windmove-left)
    (define-key map (kbd "C-M-S-<up>") #'windmove-swap-states-up)
    (define-key map (kbd "C-M-S-<right>") #'windmove-swap-states-right)
    (define-key map (kbd "C-M-S-<down>") #'windmove-swap-states-down)
    (define-key map (kbd "C-M-S-<left>") #'windmove-swap-states-left)))

;;; Tabs for window layouts (tab-bar.el and ryan-tab.el)
(ryan-emacs-builtin-package 'tab-bar
  (tab-bar-history-mode))

;;; Tab bar (lib-tab.el)
(ryan-emacs-builtin-package 'lib-tab
  (setq tab-bar-format
	'(tab-bar-format-tabs-groups
	  tab-bar-format-align-right
	  tab-bar-format-global))

  (add-hook 'after-init-hook #'ryan-tab-status-line)

  (let ((map global-map))
    (define-key map (kbd "C-x <down>") #'ryan-tab-winner-redo)
    (define-key map (kbd "C-x <up>") #'ryan-tab-winner-undo)
    (define-key map (kbd "<f8>") #'ryan-tab-status-line)
    (define-key map (kbd "C-x t t") #'ryan-tab-select-tab-dwim)))

;;; Window movement (ace-window)
(ryan-emacs-elpa-package 'ace-window
  (setq aw-keys '(?a ?r ?s ?t ?g ?h ?n ?e ?i ?o)
	aw-minibuffer-flag t)
  (ace-window-display-mode 1)

  (define-key global-map (kbd "M-o") #'ace-window))

(provide 'init-window)

;;; init-window.el ends here
