;;; vftc-moody.el --- VFTC Emacs Moody Extensions -*- lexical-binding: t -*-

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

;; This file adds some extensions to the moody plugin.

;;; Code:

(require 'vftc-common)
(require 'fontaine)
(require 'moody nil t)

(defgroup vftc-moody ()
  "Tweaks for moody"
  :group 'mode-line)

(defcustom vftc-moody-font-height-multiplier 1.65
  "Multiple of the font size to derive the moody height."
  :type 'number
  :group 'vftc-moody)

(defun vftc-moody--height ()
  "Set Moody height to an even number.
Bind this to a hook that gets called after loading/changing the
mode line's typeface (or the default one if they are the same)."
  (let* ((font (face-font 'mode-line))
         (height (truncate (* vftc-moody-font-height-multiplier (aref (font-info font) 2))))
         (height-even (if (vftc-common-number-even-p height) height (+ height 1))))
    (if font
        height-even
      24)))

(defvar moody-mode-line-height)

(defun vftc-moody--mode-line-height ()
  "Set Moody height to the value of `vftc-moody--height'."
  (let ((height (vftc-moody--height)))
    (setq moody-mode-line-height height)))

(autoload 'moody-replace-mode-line-buffer-identification "moody")
(autoload 'moody-replace-vc-mode "moody")
(autoload 'moody-replace-eldoc-minibuffer-message-function "moody")

;;;###autoload
(define-minor-mode vftc-moody-set-height
  "Toggle Moody for the mode line and configure its fonts."
  :init-value nil
  :global t
  (if vftc-moody-set-height
      (progn
        (moody-replace-mode-line-buffer-identification)
        (moody-replace-vc-mode)
        (moody-replace-eldoc-minibuffer-message-function)
        (add-hook 'fontaine-set-preset-hook #'vftc-moody--mode-line-height)
        (run-hooks 'fontaine-set-preset-hook))
    (let ((format (default-value 'mode-line-format)))
      (when (member 'moody-mode-line-buffer-identification format)
        (moody-replace-mode-line-buffer-identification 'reverse))
      (when (member '(vc-mode moody-vc-mode) format)
        (moody-replace-vc-mode 'reverse)))
    (when (eq eldoc-message-function 'moody-eldoc-minibuffer-message)
      (moody-replace-eldoc-minibuffer-message-function 'reverse))
    (remove-hook 'fontaine-set-preset-hook #'vftc-moody--mode-line-height)))

(provide 'vftc-moody)

;;; vftc-moody.el ends here
