;;; init.el --- Emacs entrypoint -*- lexical-binding: t -*-

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

;; This file sets up emacs with the core package management macros.

;;; Code:

(require 'package)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/"))

      package-archive-priorities
      '(("elpa" . 2)
	("nongnu" . 1)))

(dolist (path '("contrib-lisp" "ryan-lisp"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(setq frame-title-format '("%b")
      ring-bell-function 'ignore
      use-short-answers t
      initial-buffer-choice t)

(put 'overwrite-mode 'disabled t)

(add-hook 'package-menu-mode-hook #'hl-line-mode)

(defmacro ryan-emacs-builtin-package (package &rest body)
  "Set up builtin PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (require ,package nil 'noerror)
       (display-warning 'ryan-emacs (format "Loading `%s' failed" ,package) :warning))
     ,@body))

(defmacro ryan-emacs-elpa-package (package &rest body)
  "Set up PACKAGE from an Elisp archive with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  `(progn
     (unless (package-installed-p ,package)
       (unless package-archive-contents
	 (package-refresh-contents))
       (package-install ,package))
     (if (require ,package nil 'noerror)
         (progn ,@body)
       (display-warning 'ryan-emacs (format "Loading `%s' failed" ,package) :warning))))

(defmacro ryan-emacs-manual-package (package &rest body)
  "Set up manually installed PACKAGE with rest BODY.
PACKAGE is a quoted symbol, while BODY consists of balanced
expressions."
  (declare (indent 1))
  (let ((path (thread-last user-emacs-directory
			   (expand-file-name "contrib-lisp")
			   (expand-file-name (symbol-name (eval package))))))
    `(progn
       (eval-and-compile
         (add-to-list 'load-path ,path))
       (if (require ,package nil 'noerror)
           (progn ,@body)
         (display-warning 'ryan-emacs (format "Loading `%s' failed" ,package) :warning)
         (display-warning 'ryan-emacs (format "This must be available at %s" ,path) :warning)))))

(require 'init-core)
(require 'init-theme)
(require 'init-font)
(require 'init-modeline)
(require 'init-completion)
(require 'init-search)
(require 'init-window)
(require 'init-dired)
(require 'init-write)
(require 'init-shell)
(require 'init-git)
(require 'init-org)
(require 'init-langs)

;;; init.el ends here
