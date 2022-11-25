;;; lib-fill.el --- Fill mode extension -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan <ryan@carelesslisper.xyz>

;; Author: Ryan <ryan@carelesslisper.xyz>
;; URL: https://github.com/ryantking/system
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A better fill mode.

;;; Code:

(defgroup ryan-fill ()
  "Tweak for filling paragrahps."
  :group 'fill)

(defcustom ryan-fill-default-column 72
  "Default width for `fill-column'."
  :type 'integer
  :group 'ryan-fill)

(defcustom ryan-fill-prog-mode-column 120
  "`prog-mode' width for `fill-column'."
  :type 'integer
  :group 'ryan-fill)

(defun ryan-fill--fill-prog ()
  "Set local value of `fill-column' for programming nodes."
  (setq-local fill-column ryan-fill-prog-mode-column))

;;;###autoload
(define-minor-mode ryan-fill-mode
  "Set up fill-mode and relevant variables."
  :init-value nil
  :global t
  (if ryan-fill-mode
      (progn
        (setq-default fill-column ryan-fill-default-column)
        (add-hook 'prog-mode-hook #'ryan-fill--fill-prog)
        (add-hook 'text-mode-hook #'turn-on-auto-fill))
    (setq-default fill-column 80)
    (remove-hook 'prog-mode-hook #'ryan-fill--fill-prog)
    (remove-hook 'text-mode-hook #'turn-on-auto-fill)))

(provide 'lib-fill)

;;; lib-fill.el ends here
