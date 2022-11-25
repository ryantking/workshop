;;; init-completion.el --- Completion interfaces -*- lexical-binding: t -*-

;; Author: Ryan <ryan@carelesslisper.xyz>
;; URL: https://github.com/ryantking/system
;; Version: 0.3.0

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

;; Installs and configures the two primary completion interfaces:
;;
;; 1. Standard emacs completion via vertico
;; 2. Completion-at-point with corfu.

;;; Code:

;;; Enhanced matching (orderless.el)
(ryan-emacs-elpa-package 'orderless
  (setq orderless-component-separator 'orderless-escapable-split-on-space
	orderless-matching-styles
	'(orderless-prefixes
	  orderless-strict-leading-initialism
	  orderless-flex
	  orderless-regexp)
	orderless-style-dispatchers
	'(ryan-orderless-literal-dispatcher
	  ryan-orderless-initialism-dispatcher
	  ryan-orderless-flex-dispatcher))

  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "SPC") nil)
    (define-key map (kbd "?") nil)))

;;; Orderless extensions (lib-orderless.el)
(ryan-emacs-builtin-package 'lib-orderless)

;;; Enhanced minibuffer information (marginalia)
(ryan-emacs-elpa-package 'marginalia
  (setq marginalia-max-relative-age 0)
  (marginalia-mode 1))

;;; Minibuffer completion (minibuffer.el)
(ryan-emacs-builtin-package 'minibuffer
  (setq completion-styles '(basic orderless)
	completion-category-defaults nil
	completion-category-overrides
	'((file (styles . (basic partial-completion orderless)))
	  (project-file (styles . (basic substring partial-completion orderless)))
	  (imenu (styles . (basic substring orderless)))
	  (kill-ring (styles . (basic substring orderless)))
	  (consult-location (styles . (basic substring orderless))))
	completion-ignore-case t
	read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t
	enable-recursive-minibuffers t
	resize-mini-windows t
	minibuffer-eldef-shorten-default t
	read-answer-short t
	echo-keystrokes 0.25
	kill-ring-max 60
	minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))

  (setq-default case-fold-search t)

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (defun crm-indicator (args)
    "Add prompt indicator to `completing-read-multiple' filter ARGS."
    ;; The `error' face just makes the text red.
    (cons (concat (propertize "[CRM] " 'face 'error) (car args)) (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1))

;;; Save minibuffer history (savehist.el)
(ryan-emacs-builtin-package 'savehist
  (setq savehist-file (locate-user-emacs-file "savehist")
	history-length 10000
	history-delete-duplicates t
	savehist-save-minibuffer-history t)
  (add-hook 'after-init-hook #'savehist-mode))

;;; Enhanced selection interface (vertico.el)
(ryan-emacs-elpa-package 'vertico
  (setq vertico-scroll-margin 0
	vertico-count 10
	vertico-count-format nil
	vertico-resize nil
	vertico-cycle t

	vertico-grid-separator
	#("  |  " 2 3 (display (space :width (1))
			       ;; face (:background (face-background 'ryan-face-default-i))
			       ))

	vertico-group-format
	(concat #(" " 0 1 (face vertico-group-title))
		#(" " 0 1 (face vertico-group-separator))
		#(" %s " 0 4 (face vertico-group-title))
		#(" " 0 1 (face vertico-group-separator
				display (space :align-to (- right (-1 . right-margin) (- +1)))))))

  (vertico-mode 1)

  ;; Blend vertico with the theme
;  (set-face-attribute 'vertico-group-separator nil
;		      :strike-through t)
;  (set-face-attribute 'vertico-current nil
;		      :inherit '(ryan-face-strong ryan-face-subtle))
;  (set-face-attribute 'completions-first-difference nil
;		      :inherit '(ryan-face-default))

  (let ((map vertico-map))
    (define-key map (kbd "M-,") #'vertico-quick-insert)
    (define-key map (kbd "M-.") #'vertico-quick-exit))

  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;;; Enhanced minibuferr commands (consult.el)
(ryan-emacs-elpa-package 'consult
  (setq consult-async-input-debounce 0.5
	consult-async-input-throttle 0.8
	consult-narrow-key ">"

	consult-imenu-config
	'((emacs-lisp-mode :toplevel "Functions"
			   :types ((?f "Functions" font-lock-function-name-face)
				   (?m "Macros"    font-lock-keyword-face)
				   (?p "Packages"  font-lock-constant-face)
				   (?t "Types"     font-lock-type-face)
				   (?v "Variables" font-lock-variable-name-face))))
	;; Search C-h f for more "bookmark jump" handlers.
	consult-bookmark-narrow
	`((?d "Docview" ,#'doc-view-bookmark-jump)
	  (?e "Eshell" ,#'eshell-bookmark-jump)
	  (?f "File" ,#'bookmark-default-handler)
	  (?h "Help" ,#'help-bookmark-jump)
	  (?i "Info" ,#'Info-bookmark-jump)
	  (?m "Man" ,#'Man-bookmark-jump)
	  (?v "VC Dir" ,#'vc-dir-bookmark-jump))
	register-preview-delay 0.8
	register-preview-function #'consult-register-format
	consult-find-args "find . -not ( -wholename */.* -prune )"
	consult-preview-key 'any)

  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)

  (require 'consult-imenu)

  (let ((map global-map))
    (define-key map (kbd "C-x r b") #'consult-bookmark)
    (define-key map (kbd "C-x M-:") #'consult-complex-command)
    (define-key map (kbd "C-x M-m") #'consult-minor-mode-menu)
    (define-key map (kbd "C-x M-k") #'consult-kmacro)
    (define-key map [remap goto-line] #'consult-goto-line)
    (define-key map (kbd "M-K") #'consult-keep-lines)
    (define-key map (kbd "M-L") #'consult-focus-lines)
    (define-key map (kbd "M-s M-b") #'consult-buffer)
    (define-key map (kbd "M-s M-f") #'consult-find)
    (define-key map (kbd "M-s M-g") #'consult-ripgrep)
    (define-key map (kbd "M-s M-h") #'consult-history)
    (define-key map (kbd "M-s M-i") #'consult-imenu)
    (define-key map (kbd "M-s M-l") #'consult-line)
    (define-key map (kbd "M-s M-m") #'consult-mark)
    (define-key map (kbd "M-s M-s") #'consult-outline)
    (define-key map (kbd "M-s M-y") #'consult-yank-pop)
    (define-key map (kbd "C-x r r") #'consult-register))

  (let ((map isearch-mode-map))
    (define-key map (kbd "M-e") #'consult-isearch-history)
    (define-key map (kbd "M-s e") #'consult-isearch-history)
    (define-key map (kbd "M-s l") #'consult-line)
    (define-key map (kbd "M-s L") #'consult-line-multi))

  (let ((map minibuffer-local-map))
    (define-key map (kbd "M-s") #'consult-history)
    (define-key map (kbd "M-r") #'consult-history))

  (define-key consult-narrow-map (kbd "?") #'consult-narrow-help)

  (setq consult-after-jump-hook nil)
  (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
    (add-hook 'consult-after-jump-hook fn)))

;;; Directory switching menu (consult-dir.el)
(ryan-emacs-elpa-package 'consult-dir
  (setq consult-dir-sources '(consult-dir--source-bookmark
			      consult-dir--source-default
			      consult-dir--source-project
			      consult-dir--source-recentf))

  (dolist (map (list global-map minibuffer-local-filename-completion-map))
    (define-key map (kbd "C-x C-d") #'consult-dir)))

;;; Extended minibuffer actions (embark.el)
(ryan-emacs-elpa-package 'embark
  (setq prefix-help-command #'embark-prefix-help-command
	embark-quit-after-action t
	embark-cycle-key (kbd "C-.")
	embark-confirm-act-all nil
	embark-indicators
	'(embark-mixed-indicator
	  embark-highlight-indicator)
	embark-verbose-indicator-excluded-actions
	'("\\`customize-" "\\(local\\|global\\)-set-key"
	  set-variable embark-cycle embark-keymap-help embark-isearch)
	embark-verbose-indicator-buffer-sections
	`(target "\n" shadowed-targets " " cycle "\n" bindings)
	embark-mixed-indicator-both nil
	embark-mixed-indicator-delay 1.2
	embark-verbose-indicator-display-action nil)

  (define-key global-map (kbd "C-,") #'embark-act)
  (define-key embark-collect-mode-map (kbd "C-,") #'embark-act)

  (let ((map minibuffer-local-completion-map))
    (define-key map (kbd "C-,") #'embark-act)
    (define-key map (kbd "C-.") #'embark-export)
    (define-key map (kbd "C->") #'embark-become))
  (let ((map embark-region-map))
    (define-key map (kbd "a") #'align-regexp)
    (define-key map (kbd "i") #'epa-import-keys-region)
    (define-key map (kbd "r") #'repunctuate-sentences) ; overrides `rot13-region'
    (define-key map (kbd "s") #'sort-lines)
    (define-key map (kbd "u") #'untabify))
  (let ((map embark-symbol-map))
    (define-key map (kbd ".") #'embark-find-definition)
    (define-key map (kbd "k") #'describe-keymap)))

;;; Embark x consult
(ryan-emacs-elpa-package 'embark-consult)

;;; Embark extensions (lib-embark.el)
(ryan-emacs-builtin-package 'lib-embark
  (ryan-embark-keymaps 1))

;;; Completion for recent files (recentf.el)
(ryan-emacs-builtin-package 'recentf
  (setq recentf-save-file (locate-user-emacs-file "recentf")
	recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  (add-hook 'after-init-hook #'recentf-mode))

;;; Recentf extensions (lib-recentf.el)
(ryan-emacs-builtin-package 'lib-recentf
  (add-to-list 'recentf-keep 'ryan-recentf-keep-predicate)
  (let ((map global-map))
    (define-key map (kbd "C-x C-r") #'ryan-recentf-recent-files-or-dirs)))

  ;;; Corfu (in-buffer completion popup)
(ryan-emacs-elpa-package 'corfu
  ;; Builtin completion settings
  (setq completion-cycle-threshold 3
	read-extended-command-predicate #'command-completion-default-include-p
	tab-always-indent 'complete)

  ;; corfu settings
  (setq corfu-cycle t
	corfu-auto t
	corfu-auto-delay 0.3
	corfu-quit-at-boundary t
	corfu-quit-no-match t)

  (global-corfu-mode 1)

  (define-key corfu-map (kbd "<tab>") #'corfu-complete)

  (add-hook 'eshell-mode-hook
	    (lambda () (setq-local corfu-auto nil)))
  
  (add-hook 'minibuffer-setup-hook
	    (lambda ()
	      (unless (bound-and-true-p vertico--input)
		(setq-local corfu-auto nil) (corfu-mode 1)))))

;;; Extra completion-at-point backends (cape.el)
(ryan-emacs-elpa-package 'cape
  (require 'cape-keyword)

  (setq cape-dabbrev-min-length 3)

  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

  (dolist (backend '(cape-symbol cape-keyword cape-file cape-dabbrev))
    (add-to-list 'completion-at-point-functions backend)))


;;; Completion menu icons (kind-icon.el)
(ryan-emacs-elpa-package 'kind-icon
  (setq kind-icon-default-face 'corfu-default)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Better command line args (pcmpl-ags.el)
(ryan-emacs-elpa-package 'pcmpl-args)

;;; Dabbrev (dynamic word completion)
(ryan-emacs-builtin-package 'dabbrev
  (setq dabbrev-abbrev-char-regexp "\\W\\|\\s_"
 	dabbrev-abbrev-skip-leading-regexp "[$*/=~’]"
 	dabbrev-backward-only nil
 	dabbrev-case-distinction 'case-replace
 	dabbrev-upcase-means-case-search t)
  
  (let ((map global-map))
    (define-key map (kbd "M-/") #'dabbrev-expand)
    (define-key map (kbd "C-x M-/") #'dabbrev-completion)))

;;; Abbreviations (abbrev.el)
(ryan-emacs-builtin-package 'abbrev
  (setq abbrev-file-name (locate-user-emacs-file "abbrevs"))

  (let ((map global-map))
    (define-key map (kbd "C-x a e") #'expand-abbrev)
    (define-key map (kbd "C-x a u") #'unexpand-abbrev))

  (dolist (hook '(text-mode-hook))
    (add-hook hook #'abbrev-mode)))

(provide 'init-completion)

;;; init-completion.el ends here
