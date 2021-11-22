;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan King"
      user-mail-address "ryantking@protonmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Monoid Nerd Font" :size 14)
       doom-big-font (font-spec :family "Monoid Nerd Font" :size 24)
       doom-variable-pitch-font (font-spec :family "Monoid Nerd Font" :size 16))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)
(setq doom-nord-padded-modeline t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq undo-limit 80000000                          ;I mess up too much
      evil-want-fine-undo t                        ;By default while in insert all changes are one big blob. Be more granular
      scroll-margin 2                              ;having a little margin is nice
      auto-save-default t                          ;I dont like to lose work
      display-line-numbers-type nil                ;I dislike line numbers
      history-length 25                            ;Slight speedup
      delete-by-moving-to-trash t                  ;delete to system trash instead
      browse-url-browser-function 'xwidget-webkit-browse-url
      truncate-string-ellipsis "…")                ;default ellipses suck

(fringe-mode 0)
(global-subword-mode 1)
(tool-bar-mode 0)

(defun greedy-daemon-setup ()
  (require 'org)
  (require 'vertico)
  (require 'consult)
  (require 'marginalia))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'greedy-daemon-setup))

;; (setq vterm-always-compile-module t)

;; (setq vterm-kill-buffer-on-exit t)

(map!
  "C-<left>" #'evil-window-left
  "C-<right>" #'evil-window-right
  "C-<up>" #'evil-window-up
  "C-<down>" #'evil-window-down)

(after! evil-snipe (setq evil-snipe-scope 'visible))

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(custom-set-faces!
  `(vertical-border :background ,(doom-color 'bg) :foreground ,(doom-color 'bg)))

;; remove dividers
(when (boundp 'window-divider-mode)
  (setq window-divider-default-places nil
        window-divider-default-bottom-width 0
        window-divider-default-right-width 0)
  (window-divider-mode -1))

;; fix cursor color
(defadvice! fix-+evil-default-cursor-fn ()
  :override #'+evil-default-cursor-fn
  (evil-set-cursor-color (face-background 'cursor)))
(defadvice! fix-+evil-emacs-cursor-fn ()
  :override #'+evil-emacs-cursor-fn
  (evil-set-cursor-color (face-foreground 'warning)))

;; minimailst
;; Make a clean & minimalist frame
(use-package frame
  :config
  (setq-default default-frame-alist
                (append (list
                         '(internal-border-width . 24)
                         '(left-fringe    . 0)
                         '(right-fringe   . 0)
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         ;; '(line-spacing . 0.35)
                         '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)
  :custom
  (window-divider-default-right-width 24)
  (window-divider-default-bottom-width 12)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))

;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

;; add more to modeline
(after! doom-modeline
  (display-time-mode 1)                              ;Enable time in the mode-line
  (display-battery-mode 1)                           ;display the battery
  (setq doom-modeline-major-mode-icon t              ;Show major mode name
        doom-modeline-enable-word-count t            ;Show word count
        doom-modeline-modal-icon t                   ;Show vim mode icon
        inhibit-compacting-font-caches t))           ;Don't compact font caches

;; remove encoding indicator
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding) ;;remove encoding

;; make vertico better
(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

;; theme treemacs
(setq treemacs-width 25)
(setq doom-themes-treemacs-theme "doom-colors")
