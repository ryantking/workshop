;;; init-modeline.el --- Slightly sexier modeline -*- lexical-binding: t -*-

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

;; Uses a few plugins to provide a nicer status line.

;;; Code:

(setq mode-line-position-column-line-format '(" %l,%c")
      mode-line-defining-kbd-macro
      (propertize " Macro" 'face 'mode-line-emphasis))

(setq-default mode-line-modes
	      (seq-filter (lambda (s)
			    (not (and (stringp s)
				      (string-match-p
				       "+\\(%\\[\\|%\\]\\)$" s))))
			  mode-line-modes)

	      mode-line-format
	      '("%e"
		mode-line-front-space
		mode-line-mule-info
		mode-line-client
		mode-line-modified
		mode-line-remote
		mode-line-frame-identification
		mode-line-buffer-identification
		"  "
		mode-line-position
		mode-line-modes
		"  "
		(vc-mode vc-mode)
		"  "
		mode-line-misc-info
		mode-line-end-spaces))

(add-hook 'after-init-hook #'column-number-mode)

;;; Simple mode line configuration (moody.el)
(ryan-emacs-elpa-package 'moody)

;;; Moody extensions (lib-moody.el)
(ryan-emacs-builtin-package 'lib-moody
  (ryan-moody-set-height 1))

;;; Hide modeline "lighters" (minions.el)
(ryan-emacs-elpa-package 'minions
  (setq minions-mode-line-lighter ";"
	minions-prominent-modes
	'(defining-kbd-macro 'flymake-mode))
  (minions-mode 1))

;;; Mode line recursion indicator (recursion-indicator.el)
(ryan-emacs-elpa-package 'recursion-indicator
  (recursion-indicator-mode))

(provide 'init-modeline)

;;; init-modeline.el
