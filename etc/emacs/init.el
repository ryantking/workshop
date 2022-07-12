;;; init.el --- VFTC Emacs Entrypoint -*- lexical-binding: t -*-

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

;; This file sets up VFTC emacs with the core package management macros.

;;; Code:

;; Set ‘user-emacs-directory’ to a local location so we don’t pollute my dotfiles.
(setq user-emacs-directory (expand-file-name "emacs" (getenv "XDG_DATA_HOME")))

;; Setup ELPA and MELPA
(require 'package)

(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

(add-to-list 'package-archives '("elpa-devel" . "https://elpa.gnu.org/devel/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(defvar vftc-emacs-ensure-installed nil
  "List of package names used by `vftc-emacs-install-ensured'.")

(defun vftc-emacs-install-ensured ()
  "Install all `vftc-emacs-ensure-installed' packages, if needed.
If a package is already installed, no action is taken."
  (interactive)
  (when (yes-or-no-p (format "Try to install %d packages?" (length vftc-emacs-ensure-installed)))
    (package-refresh-contents)
    (mapc (lambda (package)
            (unless (package-installed-p package)
              (package-install package)))
          vftc-emacs-ensure-installed)))

(defmacro vftc-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'vftc-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

(defmacro vftc-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (package-installed-p ,package)
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'vftc-emacs (format "Loading `%s' failed" ,package) :warning)
       (add-to-list 'vftc-emacs-ensure-installed ,package)
       (display-warning
        'vftc-emacs
        "Run `vftc-emacs-install-ensured' to install all packages in `vftc-emacs-ensure-installed'"
        :warning))))

(defvar vftc-emacs-dir (expand-file-name "etc/emacs" (getenv "WORKSHOP_DIR")))

(defvar vftc-emacs-local-dir (expand-file-name "emacs" (getenv "XDG_DATA_HOME")))

(defvar vftc-emacs-contrib-dir (expand-file-name "usr/site-lisp" (getenv "WORKSHOP_DIR"))
  "Location of third party lisp packages.")

(defmacro vftc-emacs-manual-package (package &rest body)
  "Set up manually installed PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  (let ((path (expand-file-name (symbol-name (eval package)) vftc-emacs-contrib-dir)))
    `(progn
       (eval-and-compile
         (add-to-list 'load-path ,path))
       (if (require ,package nil 'noerror)
           (progn ,@body)
         (display-warning 'vftc-emacs (format "Loading `%s' failed" ,package) :warning)
         (display-warning 'vftc-emacs (format "This must be available at %s" ,path) :warning)))))

;; Add additional directories to `load-path'.
;; `fvtc-emacs-contrib-dir' is all third party lisp repos added as submodules.
;; "vftc-lisp" is all the lisp code I've written for my config.
(add-to-list 'load-path vftc-emacs-contrib-dir)
(add-to-list 'load-path (expand-file-name "vftc-lisp" vftc-emacs-dir))

;; The bell must be turned off
(setq ring-bell-function 'ignore)

;; Don't make me type yes
(setq use-short-answers t)

(defvar vftc-emacs-configuration-file "vftc-emacs"
  "Base name of the configuration file.")

(defun vftc-emacs--expand-file-name (file extension)
  "Return the canonical path to FILE to Emacs config with EXTENSION."
  (expand-file-name (concat file extension) (concat (getenv "WORKSHOP_DIR") "/etc/emacs")))

(defun vftc-emacs-load-config ()
  "Load main Emacs configuration, either '.el' or '.org' file."
  (let* ((init vftc-emacs-configuration-file)
         (init-el (vftc-emacs--expand-file-name init ".el"))
         (init-org (vftc-emacs--expand-file-name init ".org")))
    (require 'org)
    (if (file-exists-p init-el)
        (load-file init-el)
      (when (file-exists-p init-org)
        (org-babel-load-file init-org)))))

;; The following as for when we close the Emacs session.
(declare-function org-babel-tangle-file "ob-tangle")

(defun vftc-emacs-build-config ()
  "Produce Elisp init from my Org dotemacs.
Add this to `kill-emacs-hook', to use the newest file in the next
session.  The idea is to reduce startup time, though just by
rolling it over to the end of a session rather than the beginning
of it."
  (interactive)
  (let* ((init vftc-emacs-configuration-file)
         (init-el (vftc-emacs--expand-file-name init ".el"))
         (init-org (vftc-emacs--expand-file-name init ".org")))
    (when (file-exists-p init-el) (delete-file init-el))
    (message init-org)
    (require 'org)
    (when (file-exists-p init-org)
      (org-babel-tangle-file init-org init-el)
      (byte-compile-file init-el))))

(add-hook 'kill-emacs-hook #'vftc-emacs-build-config)

(vftc-emacs-load-config)

;;; init.el ends here
