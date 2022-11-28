;;; init-git.el --- Git interface -*- lexical-binding: t -*-

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

;; Setup git interfaces through the builtin vc-mode, diff-mode, and external
;; magit.

;;; Code:

;;; Diff viewing (diff-mode.el)
(ryan-emacs-builtin-package 'diff-mode
  (setq diff-refine t
	diff-font-lock-prettify t)

  (let ((map diff-mode-map))
    (define-key map (kbd "L") #'vc-print-root-log)
    (define-key map (kbd "v") #'vc-next-action)))

;;; Version control framework (vc.el)
(ryan-emacs-builtin-package 'vc
  (require 'vc-annotate)
  (require 'vc-dir)
  (require 'vc-git)
  (require 'add-log)
  (require 'log-view)
  (require 'log-edit)

  (setq vc-find-revision-no-save t
	vc-annotate-display-mode 'scale
	add-log-mailing-address (getenv "GITHUB_EMAIL")
	add-log-keep-changes-together t
	vc-git-diff-switches '("--path-with-stat" "--histogram")
	vc-git-print-log-follow t)

  (add-hook 'log-view-mode-hook #'hl-line-mode)

  (let ((map global-map))
    (define-key map (kbd "C-x v B") #'vc-annotate)
    (define-key map (kbd "C-x v t") #'vc-create-tag)
    (define-key map (kbd "C-x v e") #'vc-ediff)
    (define-key map (kbd "C-x v k") #'vc-delete-file)
    (define-key map (kbd "C-x v G") #'vc-log-search)
    (define-key map (kbd "C-x v d") #'vc-diff))

  (let ((map vc-dir-mode-map))
    (define-key map (kbd "t") #'vc-create-tag)
    (define-key map (kbd "O") #'vc-log-outgoing)
    (define-key map (kbd "o") #'vc-dir-find-file-other-window)
    (define-key map (kbd "d") #'vc-diff)
    (define-key map (kbd "k") #'vc-dir-delete-file)
    (define-key map (kbd "G") #'vc-revert))

  (let ((map vc-git-stash-shared-map))
    (define-key map "a" 'vc-git-stash-apply-at-point)
    (define-key map "c" 'vc-git-stash)
    (define-key map "k" 'vc-git-stash-delete-at-point)
    (define-key map "p" 'vc-git-stash-pop-at-point)
    (define-key map "s" 'vc-git-stash-snapshot))

  (let ((map vc-annotate-mode-map))
    (define-key map (kbd "M-q") #'vc-annotate-toggle-annotation-visibility)
    (define-key map (kbd "C-c C-c") #'vc-annotate-goto-line)
    (define-key map (kbd "<return>") #'vc-annotate-find-revision-at-line))

  (let ((map log-edit-mode-map))
    (define-key map (kbd "M-s") nil)
    (define-key map (kbd "M-r") nil))

  (let ((map log-view-mode-map))
    (define-key map (kbd "<tab>") #'log-view-toggle-entry-display)
    (define-key map (kbd "<return>") #'log-view-find-revision)
    (define-key map (kbd "s") #'vc-log-search)
    (define-key map (kbd "o") #'vc-log-outgoing)
    (define-key map (kbd "f") #'vc-log-incoming)
    (define-key map (kbd "F") #'vc-update)
    (define-key map (kbd "P") #'vc-pull)))

;;; VC extension functions (agitate.el)
(ryan-emacs-manual-package 'agitate
  (add-hook 'diff-mode-hook #'agitate-diff-enable-outline-minor-mode)
  (advice-add #'vc-git-push :override #'agitate-vc-git-push-prompt-for-remote)

  (setopt agitate-log-edit-informative-show-root-log t)

  (agitate-log-edit-informative-mode 1)

  (let ((map global-map))
    (define-key map (kbd "C-x v =") #'agitate-diff-buffer-or-file)
    (define-key map (kbd "C-x v g") #'agitate-vc-git-grep) 
    (define-key map (kbd "C-x v f") #'agitate-vc-git-find-revision)
    (define-key map (kbd "C-x v s") #'agitate-vc-git-show)
    (define-key map (kbd "C-x v w") #'agitate-vc-git-kill-commit-message)
    (define-key map (kbd "C-x v p p") #'agitate-vc-git-format-patch-single)
    (define-key map (kbd "C-x v p n") #'agitate-vc-git-format-patch-n-from-head))
  (let ((map diff-mode-map))
    (define-key map (kbd "C-c C-b") #'agitate-diff-refine-cycle)
    (define-key map (kbd "C-c C-n") #'agitate-diff-narrow-dwim))
  (let ((map log-view-mode-map))
    (define-key map (kbd "w") #'agitate-log-view-kill-revision)
    (define-key map (kbd "W") #'agitate-log-view-kill-revision-expanded))
  (let ((map vc-git-log-view-mode-map))
    (define-key map (kbd "c") #'agitate-vc-git-format-patch-single))
  (let ((map log-edit-mode-map))
    (define-key map (kbd "C-c C-i C-n") #'agitate-log-edit-insert-file-name)
    (define-key map (kbd "C-c C-i C-e") #'agitate-log-edit-emoji-commit)
    (define-key map (kbd "C-c C-i C-c") #'agitate-log-edit-conventional-commit)))

;;; Builtin transient (transient.el)
(ryan-emacs-builtin-package 'transient)

;;; Powerful git frontend (magit.el)
(ryan-emacs-elpa-package 'magit
  (setq magit-define-global-key-bindings nil)
  (define-key global-map (kbd "C-c g") #'magit-status)

  (require 'git-commit)
  (setq git-commit-style-convention-checks
	'(non-empty-second-line overlong-summary-l))

  (require 'magit-diff)
  (setq magit-diff-refine-hunk t)

  (require 'magit-repos)
  (setq magit-repository-directories
	'(("~/Projects" . 1))))

;;; Simple merges (smerge-mode.el)
(ryan-emacs-builtin-package 'smerge-mode)

;;; More complex diffs (ediff.el)
(ryan-emacs-builtin-package 'ediff
  (setq ediff-keep-variants nil
	ediff-show-clashes-only t
	ediff-split-window-function 'split-window-horizontally
	ediff-window-setup-function 'ediff-setup-windows-plain))

(provide 'init-git)

;;; init-git.el ends here
