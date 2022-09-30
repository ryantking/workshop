;;; vftc-faces.el --- VFTC Emacs Faces -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file defines a few faces that mirror the Nano faces.
;; They use doom theme colors to make faces that change with the theme.

;;; Code:

(defgroup vftc-faces ()
  "Faces for Very Fun to Configure Emacs."
  :group 'editing)

(defface vftc-face-bg nil
  "Background color."
  :group 'vftc-faces)

(defface vftc-face-bg-alt nil
  "Alternative background color."
  :group 'vftc-faces)

(defface vftc-face-fg nil
  "Foreground color."
  :group 'vftc-faces)

(defface vftc-face-fg-alt nil
  "Alternative foreground color."
  :group 'vftc-faces)

(defface vftc-face-base-0 nil
  "Color Base 0"
  :group 'vftc-faces)

(defface vftc-face-base-1 nil
  "Color Base 1"
  :group 'vftc-faces)

(defface vftc-face-base-2 nil
  "Color Base 2"
  :group 'vftc-faces)

(defface vftc-face-base-3 nil
  "Color Base 3"
  :group 'vftc-faces)

(defface vftc-face-base-4 nil
  "Color Base 4"
  :group 'vftc-faces)

(defface vftc-face-base-5 nil
  "Color Base 5"
  :group 'vftc-faces)

(defface vftc-face-base-6 nil
  "Color Base 6"
  :group 'vftc-faces)

(defface vftc-face-base-7 nil
  "Color Base 7"
  :group 'vftc-faces)

(defface vftc-face-base-8 nil
  "Color Base 8"
  :group 'vftc-faces)

(defface vftc-face-gray nil
  "The current theme's gray color"
  :group 'vftc-faces)

(defface vftc-face-red nil
  "The current theme's red color"
  :group 'vftc-faces)

(defface vftc-face-orange nil
  "The current theme's orange color"
  :group 'vftc-faces)

(defface vftc-face-green nil
  "The current theme's green color"
  :group 'vftc-faces)

(defface vftc-face-teal nil
  "The current theme's teal color"
  :group 'vftc-faces)

(defface vftc-face-yellow nil
  "The current theme's yellow color"
  :group 'vftc-faces)

(defface vftc-face-blue nil
  "The current theme's blue color"
  :group 'vftc-faces)

(defface vftc-face-dark-blue nil
  "The current theme's dark blue color"
  :group 'vftc-faces)

(defface vftc-face-magenta nil
  "The current theme's magenta color"
  :group 'vftc-faces)

(defface vftc-face-violet nil
  "The current theme's violet color"
  :group 'vftc-faces)

(defface vftc-face-cyan nil
  "The current theme's cyan color"
  :group 'vftc-faces)

(defface vftc-face-dark-cyan nil
  "The current theme's dark cyan color"
  :group 'vftc-faces)
  
(defface vftc-face-default nil
  "Default face."
  :group 'vftc-faces)

(defface vftc-face-default-i nil
  "Faded face with inverted colors."
  :group 'vftc-faces)

(defface vftc-face-subtle nil
  "Subtle face."
  :group 'vftc-faces)

(defface vftc-face-subtle-i nil
  "Subtle face with inverted colors."
  :group 'vftc-faces)

(defface vftc-face-strong '((t :weight bold))
  "Strong face."
  :group 'vftc-faces)

(defface vftc-face-strong-i '((t :weight bold))
  "Strong face with inverted colors."
  :group 'vftc-faces)

(defface vftc-face-salient nil
  "Salient face."
  :group 'vftc-faces)

(defface vftc-face-salient-i nil
  "Salient face with inverted colors."
  :group 'vftc-faces)

(defface vftc-face-critical nil
  "Critical face."
  :group 'vftc-faces)

(defface vftc-face-critical-i nil
  "Critical face with inverted colors."
  :group 'vftc-faces)

(defface vftc-face-faded nil
  "Faded face."
  :group 'vftc-faces)

(defface vftc-face-faded-i nil
  "Faded face with inverted colors."
  :group 'vftc-faces)

(defface vftc-face-popout nil
  "Popout face."
  :group 'vftc-faces)

(defface vftc-face-popout-i nil
  "Popout face with inverted colors."
  :group 'vftc-faces)

(defun vftc-faces-set-from-theme ()
  (set-face-attribute 'vftc-face-bg nil :foreground (doom-color 'bg))
  (set-face-attribute 'vftc-face-bg-alt nil :foreground (doom-color 'bg-alt))
  (set-face-attribute 'vftc-face-fg nil :foreground (doom-color 'fg))
  (set-face-attribute 'vftc-face-fg-alt nil :foreground (doom-color 'fg-alt))
  (set-face-attribute 'vftc-face-base-0 nil :foreground (doom-color 'base0))
  (set-face-attribute 'vftc-face-base-1 nil :foreground (doom-color 'base1))
  (set-face-attribute 'vftc-face-base-2 nil :foreground (doom-color 'base2))
  (set-face-attribute 'vftc-face-base-3 nil :foreground (doom-color 'base3))
  (set-face-attribute 'vftc-face-base-4 nil :foreground (doom-color 'base4))
  (set-face-attribute 'vftc-face-base-5 nil :foreground (doom-color 'base5))
  (set-face-attribute 'vftc-face-base-6 nil :foreground (doom-color 'base6))
  (set-face-attribute 'vftc-face-base-7 nil :foreground (doom-color 'base7))
  (set-face-attribute 'vftc-face-base-8 nil :foreground (doom-color 'base8))
  (set-face-attribute 'vftc-face-gray nil :foreground (doom-color 'grey))
  (set-face-attribute 'vftc-face-red nil :foreground (doom-color 'red))
  (set-face-attribute 'vftc-face-orange nil :foreground (doom-color 'orange))
  (set-face-attribute 'vftc-face-green nil :foreground (doom-color 'green))
  (set-face-attribute 'vftc-face-teal nil :foreground (doom-color 'teal))
  (set-face-attribute 'vftc-face-yellow nil :foreground (doom-color 'yellow))
  (set-face-attribute 'vftc-face-blue nil :foreground (doom-color 'blue))
  (set-face-attribute 'vftc-face-dark-blue nil :foreground (doom-color 'dark-blue))
  (set-face-attribute 'vftc-face-magenta nil :foreground (doom-color 'magenta))
  (set-face-attribute 'vftc-face-violet nil :foreground (doom-color 'violet))
  (set-face-attribute 'vftc-face-cyan nil :foreground (doom-color 'cyan))
  (set-face-attribute 'vftc-face-dark-cyan nil :foreground (doom-color 'dark-cyan))
  
  
  (set-face-attribute 'vftc-face-default nil
		              :foreground (doom-color 'fg)
                      :background (doom-color 'bg))
  (set-face-attribute 'vftc-face-default-i nil
		              :foreground (doom-color 'bg)
		              :background (doom-color 'fg))
  (set-face-attribute 'vftc-face-subtle nil
		              :foreground (doom-color 'base4))
  (set-face-attribute 'vftc-face-subtle-i nil
		              :foreground (doom-color 'fg)
		              :background (doom-color 'base4))
  (set-face-attribute 'vftc-face-strong nil
		              :foreground (doom-color 'fg-alt))
  (set-face-attribute 'vftc-face-strong-i nil
		              :foreground (doom-color 'bg)
		              :background (doom-color 'fg-alt))
  (set-face-attribute 'vftc-face-salient nil
		              :foreground (doom-color 'dark-blue))
  (set-face-attribute 'vftc-face-salient-i nil
		              :foreground (doom-color 'fg)
		              :background (doom-color 'dark-blue))
  (set-face-attribute 'vftc-face-critical nil
		              :foreground (doom-color 'red))
  (set-face-attribute 'vftc-face-critical-i nil
		              :foreground (doom-color 'fg)
		              :background (doom-color 'red))
  (set-face-attribute 'vftc-face-faded nil
		              :foreground (doom-color 'base6))
  (set-face-attribute 'vftc-face-faded-i nil
		              :foreground (doom-color 'bg)
		              :background (doom-color 'base6))
  (set-face-attribute 'vftc-face-popout nil
		              :foreground (doom-color 'orange))
  (set-face-attribute 'vftc-face-popout-i nil
		              :foreground (doom-color 'fg)
		              :background (doom-color 'orange)))

(provide 'vftc-faces)

;;; vftc-faces.el ends here
