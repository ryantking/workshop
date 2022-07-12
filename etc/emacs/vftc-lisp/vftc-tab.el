;;; vftc-tab.el --- VFTC Emacs Tab Bar -*- lexical-binding: t -*-

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

;; This file sets up a custom tab bar.

;;; Code:

(require 'tab-bar)

(defgroup vftc-tab ()
  "Extensions for tab-bar.el."
  :group 'tab-bar)

(defcustom vftc-tab-tab-select-num-threshold 3
  "Minimum number of tabs to prompt for numeric selection.
This is used by `vftc-tab-select-tab-dwim' to determine whether
it should prompt for completion, or to ask for just a tab number
to switch to.  If the number of open tabs is greater than this
variable's value, then the command will prompt for a number."
  :type 'integer
  :group 'vftc-tab)

;;;; General commands

(defun vftc-tab--tab-bar-tabs ()
  "Return a list of `tab-bar' tabs, minus the current one."
  (mapcar (lambda (tab)
            (alist-get 'name tab))
          (tab-bar--tabs-recent)))

;;;###autoload
(defun vftc-tab-select-tab-dwim (&optional arg)
  "Do-What-I-Mean function for getting to a `tab-bar' tab.
If no other tab exists, or with optional prefix argument
ARG (\\[universal-argument]), create one and switch to it.

If there is one other tab (so two in total) switch to it without
further questions.

If the tabs are more than `vftc-tab-tab-select-num-threshold',
show numeric hints (`tab-bar-tab-hints') and prompt for a number
to switch to.  Else prompt for full text completion."
  (interactive "P")
  (let ((tabs (vftc-tab--tab-bar-tabs)))
    (cond
     ((or arg (null tabs))
      (tab-new))
     ((length= tabs 1)
      (tab-next))
     ((length> tabs (1- vftc-tab-tab-select-num-threshold))
      (let ((tab-bar-tab-hints t)
            (bar tab-bar-mode))
        (unwind-protect
            (progn
              (unless bar
                (vftc-tab-bar-toggle 1))
              (tab-bar-select-tab
               (read-number "Go to tab NUM: ")))
          (unless bar
            (vftc-tab-bar-toggle -1)))))
     (t
      (tab-bar-switch-to-tab
       (completing-read "Select tab: " tabs nil t))))))

;;;###autoload
(define-minor-mode vftc-tab-bar-toggle
  "Toggle `tab-bar' presentation."
  :init-value nil
  :global t
  (if (or vftc-tab-bar-toggle
          (not (bound-and-true-p tab-bar-mode)))
      (progn
        (setq tab-bar-show t)
        (tab-bar-mode 1))
    (setq tab-bar-show nil)
    (tab-bar-mode -1)))

;;;; Window layout history

(declare-function winner-undo "winner")
(declare-function winner-redo "winner")

;;;###autoload
(defun vftc-tab-winner-undo ()
  "Go to previous window layout in the history.
When Tab-Bar-Mode and Tab-Bar-History-Mode are active, use
history that is specific to the current tab.  Else try to call
`winner-undo' if Winner-Mode is active.  Signal an error
otherwise."
  (interactive)
  (if (and (bound-and-true-p tab-bar-mode)
           (bound-and-true-p tab-bar-history-mode))
      (progn
        (tab-bar-history-back)
        (setq this-command 'tab-bar-history-back))
    (if (bound-and-true-p winner-mode)
        (progn
          (winner-undo)
          (setq this-command 'winner-undo))
      (user-error "No `tab-bar-history-mode' or `winner-mode' active"))))

;;;###autoload
(defun vftc-tab-winner-redo ()
  "Go to next window layout in the history.
When Tab-Bar-Mode and Tab-Bar-History-Mode are active, use
history that is specific to the current tab.  Else try to call
`winner-redo' if Winner-Mode is active.  Signal an error
otherwise."
  (interactive)
  (if (and (bound-and-true-p tab-bar-mode)
           (bound-and-true-p tab-bar-history-mode))
      (progn
        (tab-bar-history-forward)
        (setq this-command 'tab-bar-history-forward))
    (if (bound-and-true-p winner-mode)
        (progn
          (winner-redo)
          (setq this-command 'winner-redo))
      (user-error "No `tab-bar-history-mode' or `winner-mode' active"))))

;;;; Indicators for `tab-bar-format' --- EXPERIMENTAL

(defun vftc-tab-format-mule-info ()
  "Format `mode-line-mule-info' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-mule-info)) ignore)))

(defun vftc-tab-format-modified ()
  "Format `mode-line-modified' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-modified)) ignore)))

(defun vftc-tab-format-modes ()
  "Format `mode-line-modes' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-modes)) ignore)))

;; FIXME 2021-07-30: This does not update unless some other event takes
;; place, such as an ELDOC update.  Otherwise it updates every minute.
(defun vftc-tab-format-position ()
  "Format `mode-line-position' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-position)) ignore)))

(defun vftc-tab-format-vc ()
  "Format VC status for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line vc-mode)) ignore)))

(defun vftc-tab-format-misc-info ()
  "Format `mode-line-misc-info' for the tab bar."
  `((global menu-item ,(string-trim-right (format-mode-line mode-line-misc-info)) ignore)))

(defun vftc-tab-format-space-single ()
  "Format space for the tab bar."
  `((global menu-item " " ignore)))

(defun vftc-tab-format-space-double ()
  "Format double space for the tab bar."
  `((global menu-item "  " ignore)))

(defvar vftc-tab--window-divider-place (default-value 'window-divider-default-places)
  "Last value of `window-divider-default-places'.
For use in Vftc-Tab-Status-Line.")

(declare-function vftc-notmuch-mail-indicator "vftc-notmuch")

;; NOTE 2021-07-30: This is experimental and subject to review.
;;;###autoload
(define-minor-mode vftc-tab-status-line
  "Make Tab bar a status line and configure the extras.
Hide the mode lines and change their colors."
  :global t
  :group 'vftc-tab
  (if vftc-tab-status-line
      (progn
        (setq tab-bar-show t)
        (tab-bar-mode 1)
        (tab-bar-history-mode 1)
        (setq window-divider-default-places t)
        (window-divider-mode 1)
        (display-time-mode 1)
        (when (featurep 'vftc-notmuch)
          (vftc-notmuch-mail-indicator 1))
        (custom-set-faces
         `(mode-line ((default :height 1 :box nil :overline nil :underline nil)
                      (((class color) (min-colors 88) (background light))
                       :background "#FF0000" ; OR ,@(list (face-attribute 'default :foreground))
                       :foreground "#00FF00")
                      (((class color) (min-colors 88) (background dark))
                       :background "#00bcff"
                       :foreground "#00bcff")
                      (t :inverse-video t)))
         `(mode-line-inactive ((default :height 1 :box nil :overline nil :underline nil)
                               (((class color) (min-colors 88) (background light))
                                :background "red"
                                :foreground "blue")
                               (((class color) (min-colors 88) (background dark))
                                :background "green"
                                :foreground "yellow")))))
    (setq tab-bar-show nil)
    (tab-bar-mode -1)
    (tab-bar-history-mode -1)
    (setq window-divider-default-places vftc-tab--window-divider-place)
    (window-divider-mode -1)
    (display-time-mode -1)
    (custom-set-faces
     `(mode-line (( )))
     `(mode-line-inactive (( ))))))

(provide 'vftc-tab)

;;; vftc-tab.el ends here
