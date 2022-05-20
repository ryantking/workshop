;;; lib-modeline.el --- Utilities for configuring the modeline -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Ryan King <ryantking@protonmail.com>
;;
;; Author: Ryan King <ryantking@protonmail.com>
;; Maintainer: Ryan King <ryantking@protonmail.com>
;;
;; Created: 02 Apr 2022
;;
;; URL: https://github.com/ryantking/workshop/tree/master/etc/doom
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module provides functions for updating the doom modeline
;;
;;; Code:

(defun doom-modeline--pdf-icon ()
  (doom-modeline-icon
    'octicon "file-pdf" nil nil
    :face (if (doom-modeline--active)
            'all-the-icons-red
            'mode-line-inactive)
    :v-adjust 0.02))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
    (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                   '(coding-category-undecided coding-category-utf-8))
              (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
      t)))

(defun doom-modeline-update-pdf-pages ()
  "Update PDF pages."
  (setq doom-modeline--pdf-pages
    (let ((current-page-str (number-to-string (eval `(pdf-view-current-page))))
           (total-page-str (number-to-string (pdf-cache-number-of-pages))))
      (concat
        (propertize
          (concat (make-string (- (length total-page-str) (length current-page-str)) ? )
            " P" current-page-str)
          'face 'mode-line)
        (propertize (concat "/" total-page-str) 'face 'doom-modeline-buffer-minor-mode)))))


(provide 'lib-modeline)
;;; lib-modeline.el ends here
