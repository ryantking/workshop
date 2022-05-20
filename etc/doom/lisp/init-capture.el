;;; init-dashboard.el --- Org capture initialization -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Ryan King <ryantking@protonmail.com>
;;
;; Author: Ryan King <ryantking@protonmail.com>
;; Maintainer: Ryan King <ryantking@protonmail.com>
;;
;; Created: 10 May 2022
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
;; This module initializes org-capture using vulpea and doct.
;;
;;; Code:

(require 'lib-vulpea-capture)

(use-package! doct
  :commands doct)

(advice-add 'org-capture-select-template :override #'vulpea-capture-select-template-prettier)
(advice-add 'org-mks :override #'vulpea-mks-pretty)

;;;###autoload
(defun +doct-icon-declaration-to-icon (declaration)
  "Convert :icon declaration to icon"
  (let ((name (pop declaration))
         (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
         (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
         (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
    (apply set `(,name :face ,face :v-adjust ,v-adjust))))

(defun +doct-iconify-capture-templates (groups)
  "Add declaration's :icon to each template group in GROUPS."
  (let ((templates (doct-flatten-lists-in groups)))
    (setq doct-templates (mapcar (lambda (template)
                                   (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                (spec (plist-get (plist-get props :doct) :icon)))
                                     (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                              "\t"
                                                              (nth 1 template))))
                                   template)
                           templates))))

(setq doct-after-conversion-functions '(+doct-iconify-capture-templates))

(dolist (var '(vulpea-capture-inbox-file))
  (set var (expand-file-name (symbol-value var) vulpea-directory)))
(unless org-default-notes-file
  (setq org-default-notes-file vulpea-capture-inbox-file))

(setq
  org-capture-templates
  (doct `(("Quick Todo"
            :keys "t"
            :icon ("check" :set "material" :color "cyan")
            :type plain
            :file vulpea-capture-inbox-file
            :template ("* TODO %?" "%i %a"))
           ("Quick Note"
             :keys "n"
             :icon ("note" :set "material" :color "yellow")
             :type plain
             :file vulpea-capture-inbox-file
             :template ("* %?" "%i %a"))
           ("Meeting"
             :keys "m"
             :icon ("group" :set "material" :color "green")
             :type entry
             :function  ,(vulpea-make-capture-target :meeting-person "Meetings")
             :template vulpea-capture-meeting-template)
           ("Task" :keys "k"
             :icon ("inbox" :set "material" :color "blue")
             :type entry
             :function ,(vulpea-make-capture-target :task-project "Tasks")
             :template vulpea-capture-task-template
             :children (("General Task" :keys "k"
                          :icon ("inbox" :set "material" :color "blue")
                          :extra "")
                         ("Task with deadline" :keys "d"
                           :icon ("alarm" :set "material" :color "orange")
                           :extra "\nDEADLINE: %^{Deadline:}t")
                         ("Scheduled task" :keys "s"
                           :icon ("event" :set "material" :color "cyan")
                           :extra "\nSCHEDULED: %^{Start time:}t")))))

  org-roam-capture-templates
  '(("d" "default" plain "%?"
      :if-new (file+head
                "%(vulpea-subdir-select)/%<%Y%m%d%H%M%S>-${slug}.org"
                "#+title: ${title}\n\n")
      :unnarrowed t))

  org-roam-dailies-capture-templates
  `(("d" "default" entry
      "* %<%H:%M>\n\n%?"
      :if-new (file+head
                ,(expand-file-name "%<%Y-%m-%d>.org"
                   org-roam-dailies-directory)
                ,(string-join '("#+title: %<%A, %d %B %Y>"
                                 "#+filetags: journal"
                                 "\n")
                   "\n")))))

(unless (display-graphic-p)
  (add-hook 'server-after-make-frame-hook
    (defun org-capture-reinitialise-hook ()
      (when (display-graphic-p)
        (set-org-capture-templates)
        (remove-hook 'server-after-make-frame-hook
          #'org-capture-reinitialise-hook)))))

(provide 'init-capture)
