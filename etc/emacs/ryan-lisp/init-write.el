;;; init-write.el ---  -*- lexical-binding: t -*-

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

;; Packages for writing

;;; Code:

;;; Outline mode (outline.ed)
(ryan-emacs-builtin-package 'outline
  (setq outline-minor-mode-cycle t)
  (define-key global-map (kbd "<f10>") #'outline-minor-mode))

  ;;; Simple note-taking (denote)
(ryan-emacs-elpa-package 'denote
  (setq denote-directory (expand-file-name "~/Nextcloud/Documents/notes/")
	denote-known-keywords '("emacs" "work"))

  (add-hook 'dired-mode-hook #'denote-dired-mode)

  (let ((map global-map))
    (define-key map (kbd "C-c n n") #'denote)
    (define-key map (kbd "C-c n N") #'denote-type)
    (define-key map (kbd "C-c n d") #'denote-date)
    (define-key map (kbd "C-c n s") #'denote-subdirectory)
    (define-key map (kbd "C-c n i") #'denote-link)
    (define-key map (kbd "C-c n I") #'denote-link-add-links)
    (define-key map (kbd "C-c n l") #'denote-link-find-file)
    (define-key map (kbd "C-c n b") #'denote-link-backlints)
    (define-key map (kbd "C-c n r") #'denote-dired-rename-file))

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
		 '("n" "New note (with denote.el)" plain
		   (file denote-last-path)
		   #'denote-org-capture
		   :no-save t
		   :immediate-finish nil
		   :kill-buffer t
		   :jump-to-captured t))))


;;; Focus mode extensions (olivetti.el)
(ryan-emacs-elpa-package 'olivetti
  (setq olivetti-body-width 0.7
	olivetti-minimum-body-width 80))

;;; Focus mode (logos.el)
(ryan-emacs-elpa-package 'logos
  (setq logos-outlines-are-pages t)

  (setq-default logos-variable-pitch t
		logos-olivetti t)

  (let ((map global-map))
    (define-key map [remap narrow-to-region] #'logos-narrow-dwim)
    (define-key map [remap forward-page] #'logos-forward-page-dwim)
    (define-key map [remap backward-page] #'logos-backward-page-dwim)
    (define-key map (kbd "M-]") #'logos-forward-page-dwim)
    (define-key map (kbd "M-[") #'logos-backward-page-dwim)
    (define-key map (kbd "<f9>") #'logos-focus-mode)))


;;; Spell check (spell-fu.el)
(ryan-emacs-elpa-package 'spell-fu
  (setq spell-fu-directory (locate-user-emacs-file "spell-fu"))

  (define-key global-map (kbd "C-c ! S") #'spell-fu-mode))

;;; Enhanced spelling correction (lib-spell.el)
(ryan-emacs-builtin-package 'lib-spell
  (add-hook 'text-mode #'spell-fu-mode)
  (add-hook 'prog-mode #'spell-fu-mode)
  (add-hook 'conf-mode #'spell-fu-mode)
  (add-hook 'spell-fu-mode-hook #'ryan-spell-setup-spell-fu)

  (let ((map global-map))
    (define-key map (kbd "M-$") #'ryan-spell-correct)))

(provide 'init-write)

;;; init-write.el ends here
