;;; vftc-fill.el --- VFTC Emacs Fill Mode -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; A better fill mode.

;;; Code:

(defgroup vftc-fill ()
  "Tweak for filling paragrahps."
  :group 'fill)

(defcustom vftc-fill-default-column 72
  "Default width for `fill-column'."
  :type 'integer
  :group 'vftc-fill)

(defcustom vftc-fill-prog-mode-column 120
  "`prog-mode' width for `fill-column'."
  :type 'integer
  :group 'vftc-fill)

(defun vftc-fill--fill-prog ()
  "Set local value of `fill-column' for programming nodes."
  (setq-local fill-column vftc-fill-prog-mode-column))

;;;###autoload
(define-minor-mode vftc-fill-mode
  "Set up fill-mode and relevant variables."
  :init-value nil
  :global t
  (if vftc-fill-mode
      (progn
        (setq-default fill-column vftc-fill-default-column)
        (add-hook 'prog-mode-hook #'vftc-fill--fill-prog)
        (add-hook 'text-mode-hook #'turn-on-auto-fill))
    (setq-default fill-column 80)
    (remove-hook 'prog-mode-hook #'vftc-fill--fill-prog)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)))

(provide 'vftc-fill)

;;; vftc-fill.el ends here
