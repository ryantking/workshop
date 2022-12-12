;;; init-langs.el --- Language configuration -*- lexical-binding: t -*-

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

;; Sets up the configuration for different languages.

;;; Code:

;;; Paragraphs and fill-mode (lib-fill.el)
(ryan-emacs-builtin-package 'lib-fill
  (ryan-fill-mode 1)
  (add-hook 'after-init-hook #'column-number-mode))

;;; Comments (newcomment.el)
(ryan-emacs-builtin-package 'newcomment
  (setq comment-empty-lines t)
  (let ((map global-map))
    (define-key map (kbd "C-:") #'comment-kill)
    (define-key map (kbd "M-;") #'comment-indent)))

;;; Comment extensions (lib-comment.el)
(ryan-emacs-builtin-package 'lib-comment
  (let ((map global-map))
    (define-key map (kbd "C-;") 'ryan-comment-comment-dwim)
    (define-key map (kbd "C-x C-;") #'ryan-comment-timestamp-keyword)))

;;; Fast goto navigation (avy.el)
(ryan-emacs-elpa-package 'avy
  (setq avy-all-windows nil
	avy-all-windows-alt t
	avy-single-candidate-jump t
	avy-timeout-seconds 0.3
	avy-style 'pre
	avy-keys '(?a ?r ?s ?t ?g ?m ?n ?e ?i ?o))
  (define-key global-map (kbd "C-.") #'avy-goto-char-timer))

;;; Electric behaviour (electric.el)
(ryan-emacs-builtin-package 'electric
  (electric-pair-mode 1)
  (add-hook 'prog-mode-hook #'electric-indent-local-mode))

;;; Parenthesis (paren.el)
(ryan-emacs-builtin-package 'paren
  (add-hook 'after-init-hook #'show-paren-mode))

;;; Tabs and indendation
(setq-default tab-always-indent 'complete
	      tab-first-completion 'word-or-paren-or-punct)

;;; Handle long lines (so-long.el)
(ryan-emacs-builtin-package 'so-long
  (global-so-long-mode 1))

;;; Language Server Protocol (eglot.el)
(ryan-emacs-builtin-package 'eglot
  (setq eglot-autoshutdown t
	eglot-confirm-server-initiated-edits nil)

  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))
  
  (let ((map eglot-mode-map))
    (define-key map (kbd "C-c l q") #'eglot-shutdown)
    (define-key map (kbd "C-c l Q") #'eglot-shutdown-all)
    (define-key map (kbd "C-c l d") #'eglot-find-declaration)
    (define-key map (kbd "C-c l i") #'eglot-find-implementation)
    (define-key map (kbd "C-c l t") #'eglot-find-typeDefinition)
    (define-key map (kbd "C-c l r") #'eglot-rename)
    (define-key map (kbd "C-c l f") #'eglot-format-buffer)
    (define-key map (kbd "C-c l o") #'eglot-code-action-organize-imports)
    (define-key map (kbd "C-c l a") #'eglot-code-actions)))

;;; Syntax parsing (tree-sitter.el and tree-sitter-langs.el)
(ryan-emacs-elpa-package 'tree-sitter
  (ryan-emacs-elpa-package 'tree-sitter-langs)

  (set-face-attribute 'tree-sitter-hl-face:property nil
  		      :foreground (doom-color 'mangenta)
  		      :slant 'normal)

  (set-face-attribute 'tree-sitter-hl-face:method.call nil
  		      :foreground (doom-color 'dark-blue)
  		      :slant 'normal)

  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (global-tree-sitter-mode 1))


;;; Syntax checking (flymake.el)
(ryan-emacs-builtin-package 'flymake
  (setq flymake-suppress-zero-counters t
	flymake-mode-line-format
	'("" flymake-mode-line-exception flymake-mode-line-counters)
	flymake-mode-line-counter-format
	'(" " flymake-mode-line-error-counter
	  flymake-mode-line-warning-counter
	  flymake-mode-line-note-counter ""))

  (define-key global-map (kbd "C-c ! t") #'flymake-mode)

  (let ((map flymake-mode-map))
    (define-key map (kbd "C-c ! s") #'flymake-start)
    (define-key map (kbd "C-c ! d") #'flymake-show-buffer-diagnostics)
    (define-key map (kbd "C-c ! n") #'flymake-goto-next-error)
    (define-key map (kbd "C-c ! p") #'flymake-goto-prev-error)))

;;; Diagnostic popups (flymake-diagnostic-at-point.el)
(ryan-emacs-elpa-package 'flymake-diagnostic-at-point
  (setq flymake-diagnostic-at-point-display-diagnostic-function
	'flymake-diagnostic-at-point-display-popup)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;;; Additional flymake linters (flymake-collection)
(ryan-emacs-elpa-package 'flymake-collection
  (add-hook
   'shell-mode-hook
   (lambda ()
     (add-hook 'flymake-diagnostic-functions #'flymake-collection-shellcheck nil t)
     (flymake-mode 1)))

  (add-hook
   'yaml-mode-hook
   (lambda ()
     (add-hook 'flymake-diagnostic-functions #'flymake-collection-yamllint nil t)
     (flymake-mode 1))))

;;; Prose linting (flymake-vale.el)
(ryan-emacs-manual-package 'flymake-vale
  (add-hook 'text-mode-hook #'flymake-vale-load)
  (add-hook 'markdown-mode-hook #'flymake-vale-load)
  (add-hook 'org-mode-hook #'flymake-vale-load)
  (add-hook 'message-mode-hook #'flymake-vale-load))

;;; Formatting files automatically (apheleia.el)
(ryan-emacs-elpa-package 'apheleia
  (setf (alist-get 'nixfmt apheleia-formatters)
	'("alejandra"))

  (define-key global-map (kbd "C-c C-f") #'apheleia-format-buffer)

  (apheleia-global-mode 1))

;;; Documentation browser (eldoc.el)
(ryan-emacs-builtin-package 'eldoc
  (global-eldoc-mode 1))

;;; S-expression navigation (puni.el)
(ryan-emacs-elpa-package 'puni
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;;;; Additional languages

;;; Beancount support (beancount.el)
(ryan-emacs-manual-package 'beancount)

;;; C languages (rtags.el)
(ryan-emacs-elpa-package 'rtags)

;;; Cmake configuration (cmake-mode.el)
(ryan-emacs-elpa-package 'cmake-mode)

;;; Symbol handling (demangle-mode.el)
(ryan-emacs-elpa-package 'demangle-mode)

;;; Common Lisp (lisp-mode, sly, sly-macrostep, and sly-repl-ansi-color)
(ryan-emacs-builtin-package 'lisp-mode
  (setq inferior-lisp-program "sbcl")

  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(ryan-emacs-elpa-package 'sly)
(ryan-emacs-elpa-package 'sly-macrostep)
(ryan-emacs-elpa-package 'sly-repl-ansi-color)

;;; Data (csv-mode.el)
(ryan-emacs-elpa-package 'csv-mode
  (let ((map csv-mode-map))
    (define-key map (kbd "C-c a") #'csv-align-fields)
    (define-key map (kbd "C-c u") #'csv-unalign-fields)
    (define-key map (kbd "C-c s") #'csv-sort-fields)
    (define-key map (kbd "C-c S") #'csv-sort-numeric-fields)
    (define-key map (kbd "C-c k") #'csv-kill-fields)
    (define-key map (kbd "C-c t") #'csv-transpose)))

;;; Go (go-mode.el)
(ryan-emacs-elpa-package 'go-mode
  (add-hook 'go-mode-hook #'eglot-ensure)

  (let ((map go-mode-map))
    (define-key map (kbd "C-c b r") (lambda () (compile "go run .")))
    (define-key map (kbd "C-c b b") (lambda () (compile "go build")))
    (define-key map (kbd "C-c b c") (lambda () (compile "go clean")))))

;;; Go enhancements (lib-go.el)
(ryan-emacs-builtin-package 'lib-go
  (let ((map go-mode-map))
    (define-key map (kbd "C-c t t") #'ryan-go-test-rerun)
    (define-key map (kbd "C-c t a") #'ryan-go-test-all)
    (define-key map (kbd "C-c t s") #'ryan-go-test-single)
    (define-key map (kbd "C-c t n") #'ryan-go-test-nested)
    (define-key map (kbd "C-c t f") #'ryan-go-test-file)))

;;; Haskell (haskell-mode.el)
(ryan-emacs-elpa-package 'haskell-mode)

;;; Markdown (markdown-mode.el)
(ryan-emacs-elpa-package 'markdown-mode
  (setq markdown-fontify-code-blocks-natively t))

;;; Edit markdown code blocks (edit-indirect.el)
(ryan-emacs-elpa-package 'edit-indirect)

;;; Flymake markdownlint plugin (flymake-markdownlint.el)
(ryan-emacs-manual-package 'flymake-markdownlint
  (add-hook 'markdown-mode-hook #'flymake-markdownlint-setup))

;;; Enhanced markdown previews (grip-mode.el)
(ryan-emacs-elpa-package 'grip-mode
  (define-key markdown-mode-command-map (kbd "g") #'grip-mode))

;;; YAML (yaml-mode.el)
(ryan-emacs-elpa-package 'yaml-mode)

;;; Python (python-mode)
(ryan-emacs-elpa-package 'python-mode
  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-hook 'python-mode-hook #'eldoc-mode))

;;; Anaconda (anaconda-mode)
(ryan-emacs-elpa-package 'anaconda-mode
  (add-hook 'python-mode-hook #'anaconda-mode))

;;; Caddyfile (caddyfile-mode.el)
(ryan-emacs-elpa-package 'caddyfile-mode)

;;; Terraform (terraform-mode.el)
(ryan-emacs-elpa-package 'terraform-mode)

;;; Plain text (text-mode.el)
(ryan-emacs-builtin-package 'text-mode
  (add-to-list 'auto-mode-alist '("\\(README\\|CHANGELOG\\|COPYING\\|LICENSE\\)\\'" . text-mode)))

;;; Fennel (fennel-mode.el)
(ryan-emacs-elpa-package 'fennel-mode)

(ryan-emacs-elpa-package 'haskell-mode
  (add-hook 'haskell-mode #'eglot-ensure))

;;; Ocaml
(ryan-emacs-elpa-package 'tuareg
  (setq tuareg-prettify-symbols-full t)
  (add-hook 'tuareg-mode-hook #'eglot-ensure)

  (let ((map tuareg-mode-map))
    (define-key map (kbd "C-c m a") #'tuareg-find-alternate-file)))

(ryan-emacs-elpa-package 'utop
  (add-hook 'tuareg-mode-hook #'utop-minor-mode))

(add-to-list 'load-path (expand-file-name "~/.opam/default/share/emacs/site-lisp"))
(ryan-emacs-manual-package 'ocp-indent
  (add-hook 'tuareg-mode-hook #'ocp-setup-indent))

(provide 'init-langs)

;;; init-langs ends here
