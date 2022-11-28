;;; init-theme.el --- Theme and other visual Xtras -*- lexical-binding: t -*-

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

;; Installs the theme and other visual plugins.

;;; Code:

;;; Frame configuration
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon nil
	frame-title-format nil))


;;; Base theme pack (doom-themes.el)
(ryan-emacs-elpa-package 'doom-themes
  (load-theme 'doom-nord-aurora t))

;;;; Custom Faces

(defgroup ryan-faces ()
  "Custom emacs faces."
  :group 'editing)

(defface ryan-pulsar
  `((t :background ,(doom-color 'yellow)))
  "Pulsar face that uses the Doom theme color."
  :group 'vftc-faces)

(defface ryan-pulsar-highlight
  `((t :background ,(doom-color 'red)))
  "Pulsar highlight face that uses the Doom theme color."
  :group 'ryan-faces)

(defface ryan-lin
  `((t :background ,(doom-color 'base4)))
  "A `lin' face that uses the Doom theme color."
  :group 'ryan-faces)

;;;; Extras

;;; Highlight lines (pulsar)
(ryan-emacs-elpa-package 'pulsar
  (setq pulsar-functions '(forward-page backward-page evil-scroll-page-down evil-scroll-page-up)
	pulsar-pulse-on-window-change t
	pulsar-pulse t
	pulsar-delay 0.055
	pulsar-iterations 10
	pulsar-face 'ryan-pulsar
	pulsar-highlight-face 'ryan-pulsar-highlight)

  (pulsar-global-mode 1)

  (let ((map global-map))
    (define-key map (kbd "C-x l") #'pulsar-pulse-line)
    (define-key map (kbd "C-x L") #'pulsar-highlight-dwim)))

;;; Highlight selection (lin.el)
(ryan-emacs-elpa-package 'lin
  (setq lin-face 'ryan-lin
	lin-mode-hooks
	'(
	  ;; bongo-mode-hook
	  dired-mode-hook
	  elfeed-search-mode-hook
	  git-rebase-mode-hook
	  ibuffer-mode-hook
	  ;; ilist-mode-hook
	  ;; ledger-report-mode-hook
	  log-view-mode-hook
	  ;; magit-log-mode-hook
	  ;; mu4e-headers-mode
	  ;; notmuch-search-mode-hook
	  ;; notmuch-tree-mode-hook
	  ;; occur-mode-hook
	  org-agenda-mode-hook
	  ;; tabulated-list-mode-hook
	  ))
  (lin-global-mode 1))

;;; Preview colors (rainbow-mode.el)
(ryan-emacs-elpa-package 'rainbow-mode)

;;; Fringe mode
(ryan-emacs-builtin-package 'fringe
  (fringe-mode nil))

;;; Line numbers and relevant indicators (ryan-sideline.el)
(ryan-emacs-builtin-package 'lib-sideline
  (require 'display-line-numbers)
  (setq display-line-numbers-major-tick 50
	display-line-numbers-minor-tick 10
	display-line-numbers-widen t)

  (ryan-emacs-elpa-package 'diff-hl
    (setq diff-hl-draw-borders nil))

  (require 'hl-line)
  (setq hl-line-sticky-flag nil)

  (require 'whitespace)

    (set-face-attribute 'line-number-major-tick nil
		      :foreground (doom-color 'base6)
		      :background nil)

  (set-face-attribute 'line-number-minor-tick nil
		      :foreground (doom-color 'base4)
		      :background nil)

  (add-hook 'prog-mode-hook #'ryan-sideline-mode)

  (let ((map global-map))
    (define-key map (kbd "<f6>") #'ryan-sideline-negative-space-toggle)
    (define-key map (kbd "<f7>") #'ryan-sideline-mode)
    (define-key map (kbd "C-c z") #'delete-trailing-whitespace)))

;;; Cursor appearance (cursory.el)
(ryan-emacs-elpa-package 'cursory
  (setq cursory-presets
	'((bar
	   :cursor-type (bar . 2)
	   :cursor-in-non-selected-windows hollow
	   :blink-cursor-blinks 10
	   :blink-cursor-interval 0.5
	   :blink-cursor-delay 0.2)
	  (box
	   :cursor-type box
	   :cursor-in-non-selected-windows hollow
	   :blink-cursor-blinks 10
	   :blink-cursor-interval 0.5
	   :blink-cursor-delay 0.2)
	  (underscore
	   :cursor-type (hbar . 3)
	   :cursor-in-non-selected-windows hollow
	   :blink-cursor-blinks 50
	   :blink-cursor-interval 0.2
	   :blink-cursor-delay 0.2)))

  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))

  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

  (define-key global-map (kbd "C-c p") #'cursory-set-preset))

(provide 'init-theme)

;;; init-theme.el ends here
