;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

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

;; This file sets Emacs behavior before package managements starts.

;;; Code:

;; Initialize installed packages
(setq package-enable-at-startup t)

(defvar package-quickstart)

;; Use the package cache
(setq package-quickstart t)

;; Don't resize the frame
(setq frame-inhibit-implied-resize t)

;; Disable GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-splash-screen t
      use-dialog-box t
      use-file-dialog nil)

(setq inhibit-startup-echo-area-message user-login-name
      inhibit-startup-screen t
      inhibit-startup-buffer-menu t)

;; Don't spam compilation warnings
(setq native-comp-async-report-warnings-errors 'silent)

;; Temporarily disable garbage collection
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

;; Move elpa
(setq package-user-dir (expand-file-name "emacs/elpa" (getenv "XDG_DATA_HOME")))

;;; early-init.el ends here
