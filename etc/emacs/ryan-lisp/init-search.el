;;; init-search.el --- Search configurations -*- lexical-binding: t -*-

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

;; This file configures several different bits of search functionality:
;;
;; - Incremental buffer search and extensions (isearch.el and lib.search.el)
;; - Buffer replacements (replace.el)
;; - Emacs' builtin grep (grep.el)
;; - Writeable grep buffers (wgrep.el)
;; - Symbol cross referencing (xref.el)
;; - Bookmarks (bookmark.el)

;;; Code:

;;; Isearch, occur, grep, and extras (isearch.el)
(ryan-emacs-builtin-package 'isearch
  (setq search-whitespace-regexp ".*?"
	isearch-lazy-count t
	lazy-count-prefix-format " (%s/%s)"
	isearch-yank-on-move 'shift
	isearch-allow-scroll t
	isearch-repeat-on-direction-change t)

  (let ((map isearch-mode-map))
    (define-key map (kbd "C-g") #'isearch-cancel)
    (define-key map (kbd "M-/") #'isearch-complete)))

;;; Replace mode (replace.el)
(ryan-emacs-builtin-package 'replace
  (add-hook 'occur-mode-hook #'hl-line-mode)
  (add-hook 'occur-mode-hook #'ryan-common-truncate-lines-silently)
  (define-key occur-mode-map (kbd "t") #'toggle-truncate-lines))

;;; Emacs grep (grep.el)
(ryan-emacs-builtin-package 'grep)

;;; Extensions to search (lib-search.el)
(ryan-emacs-builtin-package 'lib-search
  (let ((map global-map))
    (define-key map (kbd "M-s %") #'ryan-search-isearch-replace-symbol)
    (define-key map (kbd "M-s M-%") #'ryan-search-replace-markup)
    (define-key map (kbd "M-s M-<") #'ryan-search-isearch-beginning-of-buffer)
    (define-key map (kbd "M-s g") #'ryan-search-grep)
    (define-key map (kbd "M-s u") #'ryan-search-occur-urls)
    (define-key map (kbd "M-s t") #'ryan-search-grep-todo-keywords)
    (define-key map (kbd "M-s M-t") #'ryan-search-grep-todo-keywords))
  (let ((map isearch-mode-map))
    (define-key map (kbd "<up>") #'ryan-search-isearch-repeat-backward)
    (define-key map (kbd "<down>") #'ryan-search-isearch-repeat-forward)
    (define-key map (kbd "<backpace>") #'ryan-search-isearch-abort-dwim)
    (define-key map (kbd "<C-return>") #'ryan-search-isearch-other-end)))

;;; Regular expression builder (re-builder.el)
(ryan-emacs-builtin-package 're-builder)

;;; Writeable grep (wgrep.el)
(ryan-emacs-elpa-package 'wgrep
  (setq wgrep-auto-save-buffer t
	wgrep-change-readonly-file t)
  (let ((map grep-mode-map))
    (define-key map (kbd "e") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-x C-q") #'wgrep-change-to-wgrep-mode)
    (define-key map (kbd "C-c C-C") #'wgrep-finish-edit)))

;;; Cross references (xref.el)
(ryan-emacs-builtin-package 'xref
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
	xref-show-xrefs-function #'xref-show-definitions-buffer
	xref-search-program 'ripgrep))

;;; Builtin bookmarking (bookmark.el)
(ryan-emacs-builtin-package 'bookmark
  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode))

;;; Project management (project.el)
(ryan-emacs-builtin-package 'project
  (setq project-switch-commands
	'((?f "File" project-find-file)
	  (?s "Subdir" ryan-project-find-subdir)
	  (?g "Grep" project-find-regexp)
	  (?d "Dired" project-dired)
	  (?b "Buffer" project-switch-to-buffer)
	  (?q "Query replace" project-query-replace-regexp)
	  (?t "Tag switch" ryan-project-retrieve-tag)
	  (?v "VC dir" project-vc-dir)
	  (?l "Log VC" ryan-project-commit-log)
	  (?e "Eshell" project-eshell)))
  (define-key global-map (kbd "C-x p q") #'project-query-replace-regexp))

;;; Project extensions (project-x.el)
(ryan-emacs-manual-package 'project-x
  (setq project-x-window-list-file (locate-user-emacs-file "project-x-window-list"))
  (project-x-mode 1))

(provide 'init-search)

;;; init-search.el ends here
