;;; init-agenda.el --- Initialization logic for the org agenda -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2015-2022, Ryan King <ryantking@protonmail.com>
;;
;; Author: Ryan King <ryantking@protonmail.com>
;; Maintainer: Ryan King <ryantking@protonmail.com>
;;
;; Created: 12 May 2022
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
;; This module initializes the org agenda
;;
;;; Code:

(require 'lib-vulpea-agenda)

(use-package! org-super-agenda
  :commands org-super-agenda-mode)

(org-super-agenda-mode)
(advice-add 'org-agenda :before #'vulpea-agenda-files-update)

(setq
  org-agenda-skip-scheduled-if-done t
  org-agenda-skip-deadline-if-done t
  org-agenda-include-deadlines t
  org-agenda-inhibit-startup t
  org-agenda-log-mode-updates '(closed clock state)
  org-agenda-show-inherited-tags nil
  org-agenda-block-separator nil
  org-agenda-dim-blocked-tasks nil
  org-agenda-compact-blocks t
  org-agenda-tags-column 0
  org-agenda-window-setup 'current-window

  org-agenda-prefix-format
  '((agenda . " %(vulpea-agenda-category 24) %?-12t %12s")
     (todo . " %(vulpea-agenda-category 24) ")
     (tags . " %(vulpea-agenda-category 24) ")
     (search . " %(vulpea-agenda-category 24) "))

  org-todo-keyword-format "%-1s"

  org-agenda-span 'day
  org-agenda-start-day nil
  org-super-agenda-groups
  '((:name "Today"
      :scheduled today
      :time-grid t
      :order 1)
     (:name "Due Today"
      :deadline today
      :order 2)
     (:name "Overdue"
       :deadline past
       :order 3)
     (:name "Due Soon"
       :deadline future
       :order 4))

  org-agenda-custom-commands
  `(("o" "Overview"
      ((agenda "" ((org-agenda-span 'day)
                    (org-super-agenda-groups
                      '((:name "Today"
                          :time-grid t
                          :date today
                          :scheduled today
                          :order 1)))))
        (alltodo "" ((org-agenda-overriding-header "")
                      (org-super-agenda-groups
                        '((:name "Next to do"
                            :todo "NEXT"
                            :order 1))
                        )))
        ))

     ("v" "Vulpea"
      (,vulpea-agenda-cmd-refile
        ,vulpea-agenda-cmd-today
        ,vulpea-agenda-cmd-focus
        ,vulpea-agenda-cmd-waiting)
      ((org-agenda-buffer-name vulpea-agenda-main-buffer-name)))
     ))

(provide 'init-agenda)

;;; init-agenda.el ends here
