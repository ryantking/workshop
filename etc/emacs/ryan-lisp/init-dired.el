;;; init-dired.el --- Dired mode setup -*- lexical-binding: t -*-

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

;;

;;; Code:

;;; The directory editor (dired.el)
(ryan-emacs-builtin-package 'dired
  (setq dired-recursive-copies 'always
	dired-recursive-deletes 'always
	delete-by-moving-to-trash nil
	dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"
	dired-dwim-target t
	dired-auto-revert-buffer #'dired-directory-changed-p)
  
  (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  (add-hook 'dired-mode-hook #'hl-line-mode))

;;; Builtin dired helpers (dired-aux.el)
(ryan-emacs-builtin-package 'dired-aux
  (setq dired-isearch-filenames 'dwim
	dired-create-destination-dirs t
	dired-vc-rename-file t
	dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir))))
  (let ((map dired-mode-map))
    (define-key map (kbd "C-+") #'dired-create-empty-file)
    (define-key map (kbd "M-s f") #'nil)
    (define-key map (kbd "C-x v v") #'dired-vc-next-action)))

;;; Dired extensions (dired-x.el)
(ryan-emacs-builtin-package 'dired-x)

;;; Custom dired extensions (lib-dired.el)
(ryan-emacs-builtin-package 'lib-dired
  (add-hook 'dired-mode-hook #'ryan-dired-setup-imenu)

  (let ((map dired-mode-map))
    (define-key map (kbd "i") #'ryan-dired-insert-subdir)
    (define-key map (kbd "/") #'ryan-dired-limit-regexp)
    (define-key map (kbd "C-c C-l") #'ryan-dired-limit-regexp)
    (define-key map (kbd "M-n") #'ryan-dired-subdirectory-next)
    (define-key map (kbd "C-c C-n") #'ryan-dired-subdirectory-next)
    (define-key map (kbd "M-p") #'ryan-dired-subdirectory-previous)
    (define-key map (kbd "C-c C-p") #'ryan-dired-subdirectory-previous)
    (define-key map (kbd "M-s G") #'ryan-dired-grep-marked-files)))

;;; Browse dired subtrees (dired-subtree.el)
(ryan-emacs-elpa-package 'dired-subtree
  (setq dired-subtree-use-backgrounds nil)
  (let ((map dired-mode-map))
    (define-key map (kbd "<tab>") #'dired-subtree-toggle)
    (define-key map (kbd "<backtab>") #'dired-subtree-remove)))

;;; Rename files in dired (wdired.el)
(ryan-emacs-builtin-package 'wdired
  (setq wdired-allow-to-redirect-links t))

;;; View images in dired (image-dired.el)
(ryan-emacs-builtin-package 'image-dired
  (define-key image-dired-thumbnail-mode-map (kbd "<return>") #'image-dired-thumbnail-display-external))

;;; Dired-like interface for the traysh (trashed.el)
(ryan-emacs-elpa-package 'trashed
  (setq trashed-action-confirmer 'y-or-n-p
	trashed-date-format "%Y-%m-%d %H:%M:%S"))

;;; Dired-like interface for buffers (ibuffer.el)
(ryan-emacs-builtin-package 'ibuffer
  (setq ibuffer-expert t
	ibuffer-use-other-window nil
	ibuffer-default-sorting-mode 'filename/process)
  (add-hook 'ibuffer-mode-hook #'hl-line-mode)
  (define-key global-map (kbd "C-x C-b") #'ibuffer)
  (let ((map ibuffer-mode-map))
    (define-key map (kbd "* f") #'ibuffer-mark-by-file-name-regexp)
    (define-key map (kbd "* g") #'ibuffer-mark-by-name-regexp)
    (define-key map (kbd "s n") #'ibuffer-do-sort-by-alphabetic)
    (define-key map (kbd "/ g") #'ibuffer-filter-by-content)))

;;; Dired-like list for registers (rlist.el)
(ryan-emacs-manual-package 'rlist
  (setq rlist-expert t
	rlist-verbose t)

  (let ((map global-map))
    (define-key map (kbd "C-x r <backpace>") #'rlist-list-registers)
    (define-key map (kbd "C-x r DEL") #'rlist-list-registers)))

;;; Dired-like lists (ilist.el)
(ryan-emacs-elpa-package 'ilist)

;;; Dired like bookmarks view (blist.el)
(ryan-emacs-elpa-package 'blist
  (setq blist-expert t)

  (blist-define-criterion "info" "Info"
			  (eq (bookmark-get-handler bookmark)
			      #'Info-bookmark-jump))

  (setq blist-filter-groups
	(list
	 (cons "Eshell" #'blist-eshell-p)
	 (cons "Info" #'blist-info-p)
	 (cons "Default" #'blist-default-p)))

  (define-key global-map (kbd "C-x r l") #'blist-list-bookmarks))


(provide 'init-dired)

;;; init-dired.el ends here
