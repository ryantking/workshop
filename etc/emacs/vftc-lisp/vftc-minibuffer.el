;;; vftc-minibuffer.el --- Minibuffer library -*- lexical-binding: t -*-

;; Copyright (C) 2022 Ryan King

;; Author: Ryan King <ryantking@protonmail.com>
;; URL: https://github.com/ryantking/workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; A library for a nicer looking mini buffer
;;
;;; Code:

(require 'mini-frame)

(defgroup vftc-minibuffer nil
  "VFTC Emacs modeline"
  :group 'visual)

(defun vftc-minibuffer--header ()
  "Header line function for the minibuffer"
  (let* ((depth (minibuffer-depth))
         (left (concat " ☰ " (if (> depth 1) (format "Minibuffer (%d)" depth) "Minibuffer")))
         (right "C-g: abort")
         (spacer (propertize " " 'display `(space :align-to (- right (-1 . right-margin) ,(- (length right) -1))))))
    (concat left spacer right "\n")))

(defun vftc-minibuffer-setup ()
  "Install a header line in the minibuffer via an overlay (and a hook)"
  (set-window-margins nil 0 0)
  (set-fringe-style '(0 . 0))
  (cursor-intangible-mode t)
  (face-remap-add-relative 'default :inherit 'vftc-face-default)
  (let* ((overlay (make-overlay (+ (point-min) 0) (+ (point-min) 0)))
         (inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (insert (propertize
               (concat (propertize (vftc-minibuffer--header)
                                   'face 'vftc-face-subtle-i)
                       (propertize "\n" 'face `(:height 0.33))
                       (propertize " "))
               'cursor-intangible t
               'read-only t
               'field t
               'rear-nonsticky t
               'front-sticky t)))))

(defun vftc-minibuffer--frame-parameters ()
  "Compute minibuffer frame size and position."

  ;; Quite precise computation to align the minibuffer and the
  ;; modeline when they are both at top position
  (let* ((edges (window-pixel-edges)) ;; (left top right bottom)
         (body-edges (window-body-pixel-edges)) ;; (left top right bottom)
         (left (nth 0 edges)) ;; Take margins into account
         (top (nth 1 edges)) ;; Drop header line
         (right (nth 2 edges)) ;; Take margins into account
         (bottom (nth 3 body-edges)) ;; Drop header line
         (left (if (eq left-fringe-width 0) left (- left (frame-parameter nil 'left-fringe))))
         (right (nth 2 edges))
         (right (if (eq right-fringe-width 0) right (+ right (frame-parameter nil 'right-fringe))))
         (border 1)
         (width (- right left (* 0 border)))

         ;; Window divider mode
         (width (- width (if (and (bound-and-true-p window-divider-mode)
                                  (or (eq window-divider-default-places 'right-only)
                                      (eq window-divider-default-places t))
                                  (window-in-direction 'right (selected-window)))
                             window-divider-default-right-width
                           0)))
         (y (- top border)))


    (append `((left-fringe . 0)
              (right-fringe . 0)
              (user-position . t)
              (foreground-color . ,(face-foreground 'vftc-face-default nil 'default))
              (background-color . ,(face-background 'vftc-face-default nil 'default)))
            `((left . ,(- left border))
              (top . ,y)
              (width . (text-pixels . ,width))
              (child-frame-border-width . ,border)
              (internal-border-width . ,border)))))

(defun vftc-minibuffer--activate ()
  "Activate vftc-minibuffer minor mode"

  ;; Mini-frame setup
  (set-face-background 'child-frame-border (face-foreground 'vftc-face-faded))
  (setq mini-frame-default-height 3
        mini-frame-create-lazy t
        mini-frame-show-parameters 'vftc-minibuffer--frame-parameters
        mini-frame-ignore-commands '("edebug-eval-expression" debugger-eval-expression)
        mini-frame-internal-border-color (face-foreground 'vftc-face-faded)
        mini-frame-resize-min-height 3)

  ;; This does not work unless it is set via customize-variable
  (setq mini-frame-resize t)

  (add-hook 'minibuffer-setup-hook #'vftc-minibuffer-setup)
  (mini-frame-mode 1))

(defun vftc-minibuffer--deactivate ()
  "Deactivate vftc-minibuffer minor mode"

  (remove-hook 'minibuffer-setup-hook #'vftc-minibuffer-setup)
  (mini-frame-mode -1))

;;;###autoload
(define-minor-mode vftc-minibuffer-mode
  "Toggle vftc-minibuffer minor mode"
  :group 'vftc-minibuffer
  :global t
  :init-value nil

  ;; Toggle mode
  (if vftc-minibuffer-mode
      (vftc-minibuffer--activate)
    (vftc-minibuffer--deactivate))

  ;; Run any registered hooks
  (run-hooks 'vftc-minibuffer-mode-hook))

(provide 'vftc-minibuffer)

;;; end of vftc-minibuffer.el
