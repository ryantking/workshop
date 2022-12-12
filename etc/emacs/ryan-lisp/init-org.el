;;; init-org.el --- Org workflow -*- lexical-binding: t -*-

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

;; My GTD org mode workflow.

;;; Code:

;;; Information manager (org-mode)
(ryan-emacs-builtin-package 'org
  (setq org-directory (expand-file-name "~/Nextcloud/Documents/org")
	org-imenu-depth 7
	org-special-ctrl-a/e t
	org-special-ctrl-k t
	org-M-RET-may-split-line '((default . nil))
	org-hide-emphasis-markers t
	org-pretty-entities t
	org-hide-leading-stars t
	org-startup-folded 'content
	org-structure-template-alist
	'(("s" . "src")
	  ("E" . "src emacs-lisp")
	  ("e" . "example")
	  ("q" . "quote")
	  ("v" . "verse")
	  ("V" . "verbatim")
	  ("c" . "center")
	  ("C" . "comment"))
	org-catch-invisible-edits 'show
	org-loop-over-headlines-in-active-region 'start-level
	org-modules '(org-crypt org-eshell)
	org-use-sub-superscripts '{}
	org-insert-heading-respect-content t)

    ;;; todo and refile
  (setq org-capture-templates
	'(("i" "Inbox" entry (file "inbox.org") "* TODO %?\n%U\n\n  %i")
	  ("l" "Link" entry (file "inbox.org") "* TODO %?\n%U\n\n  %i\n  %a")
	  ("p" "Project" entry (file+headline "tasks.org") "* %? [/]")
	  ("m" "Meeting" entry (file+headline "tasks.org" "Future") "* %? :meeting:\n%^T")
	  ("n" "Note" entry (file "notes.org") "* Note (%a)\n%U\n\n%?"))
	org-refile-targets
	'(("tasks.org" . (:level . 1))
	  ("projects.org" . (:level . 1)))
	org-refile-use-outline-path 'file
	org-outline-path-complete-in-steps nil
	org-refile-allow-creating-parent-nodes 'confirm
	org-refile-use-cache t
	org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
	  (sequence "HOLD(h)" "|" "CNCL(c)"))
	org-todo-keyword-faces
	`(("NEXT" . ,(doom-color 'cyan))
	  ("HOLD" . ,(doom-color 'yellow))
	  ("CNCL" . ,(doom-color 'base4)))
	org-use-fast-tag-selection 'expert
	org-priority-faces
	'((?A . '(bold org-priority))
	  (?B . org-priority)
	  (?C . '(shadow org-priority)))
	org-fontify-quote-and-verse-blocks t
	org-fontify-whole-block-delimiter-line t
	org-enforce-todo-dependencies t
	org-enforce-todo-checkbox-dependencies t
	org-track-ordered-property-with-tag t
	org-highest-priority ?A
	org-lowest-priority ?C
	org-default-priority ?A)

    ;;; tags
  (setq org-tag-alist
	'(("@home" . ?H)
	  ("@work" . ?W)
	  ("@computer" . ?C)
	  ("@web" . ?w)
	  ("@errand" . ?E)
	  ("emacs" . ?e)
	  ("mac" . ?m)
	  ("linux" . ?l)
	  ("homelab" . ?h)))

    ;;; log
  (setq org-log-done 'time
	org-log-into-drawer nil
	org-read-date-prefer-future 'time)

  (add-hook 'org-after-todo-state-change-hook
	    (lambda (&rest ignore)
	      (when (and (string= (org-get-todo-state) "NEXT")
			 (not (org-entry-get nil "ACTIVATED")))
		(org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]")))))

    ;;; agenda
  (setq org-default-notes-file (expand-file-name "notes.org" org-directory)
	org-agenda-files `("tasks.org" "projects.org" ,org-default-notes-file)
	org-agenda-start-on-weekday 0
	org-agenda-prefix-format
	'((agenda . " %i %?-12t% s")
	  (todo   . " %i")
	  (tags   . " %i %-12:c")
	  (search . " %i %-12:c"))
	org-agenda-window-setup 'current-window
	org-agenda-bulk-mark-char "#"
	org-agenda-follow-indirect t
	org-agenda-hide-tags-regexp "."
	org-deadline-warning-days 5
	org-agenda-skip-scheduled-if-deadline-is-shown t
	org-agenda-skip-timestamp-if-deadline-is-shown t
	org-agenda-skip-deadline-prewarning-if-scheduled 1
	org-agenda-search-headline-for-time nil
	org-scheduled-past-days 365
	org-deadline-past-days 365
	org-agenda-time-leading-zero t
	org-agenda-time-grid
	'((daily today require-timed)
	  (0600 0700 800 0900 1000 1100
		1200 1300 1400 1500 1600
		1700 1800 1900 2000 2100)
	  " ....." "-----------------")
	org-agenda-todo-ignore-with-date t
	org-agenda-todo-ignore-timestamp t
	org-agenda-todo-ignore-scheduled t
	org-agenda-todo-ignore-deadlines t
	org-agenda-todo-ignore-time-comparison-use-seconds t

	org-agenda-custom-commands
	'(("d" "Dashboard"
	   ((agenda "" ((org-agenda-span 1)
			(org-deadline-warning-days 0)
			(org-scheduled-past-days 0)
			(org-agenda-format-date "%A %-e %B %Y")
			(org-agenda-overriding-header "Today's agenda\n")))
	    (agenda "" ((org-agenda-start-on-weekday nil)
			(org-agenda-start-day "+1d")
			(org-agenda-span 3)
			(org-deadline-warning-days 0)
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-overriding-header "Next three days\n")))
	    (agenda "" ((org-agenda-time-grid nil)
			(org-agenda-start-on-weekday nil)
			(org-agenda-start-day "+4d")
			(org-agenda-span 15)
			(org-agenda-show-all-dates nil)
			(org-deadline-warning-days 0)
			(org-agenda-entry-types '(:deadline))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
			(org-agenda-overriding-header "Upcoming deadlines")))
	    (todo "NEXT"
		  ((org-agenda-max-todos nil)
		   (org-agenda-overriding-header "Next Actions\n")))
	    (todo "TODO"
		  ((org-agenda-overriding-header "Inbox")
		   (org-agenda-files '("inbox.org"))
		   (org-agenda-text-search-extra-files nil)))))
	  ("n" "Today and all next tasks"
	   ((agenda "" ((org-agenda-span 1)
			(org-deadline-warning-days 0)
			(org-scheduled-past-days 0)
			(org-agenda-format-date "%A %-e %B %Y")
			(org-agenda-overriding-header "Today's agenda\n")))
	    (todo "NEXT"
		  ((org-agenda-overriding-header "Next Actions\n")))
	    (todo "HOLD" ((org-agenda-todo-ignore-with-date)
			  (org-agenda-overriding-header "Blocked items")))))))

  (advice-add 'org-refile :after
	      (lambda (&rest _)
		(ryan-org-save-all)))

  (ryan-emacs-builtin-package 'org-habit
    (setq org-habit-graph-column 50
	  org-habit-preceding-days 9))

    ;;; source blocks
  (setq org-confirm-babel-evaluate nil
	org-src-window-setup 'current-window
	org-edit-src-persistent-message nil
	org-src-preserve-indentation t
	org-edit-src-content-indentation 0)

    ;;; exports
  (setq org-export-headline-levels 8
	org-export-backends '(html latex texinfo md))

    ;;; IDs
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  (run-at-time (* 60 5) nil #'org-agenda-to-appt)

  (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
    (add-hook hook #'pulsar-recenter-middle)
    (add-hook hook #'pulsar-reveal-entry))

  (let ((map global-map))
    (define-key map (kbd "C-c a") #'org-agenda)
    (define-key map (kbd "C-c c") #'org-capture)
    (define-key map (kbd "C-c i") #'ryan-org-capture-inbox)
    (define-key map (kbd "C-c L") #'org-store-link)
    (define-key map (kbd "C-c o") #'org-open-at-point-global))

  (let ((map org-mode-map))
    (define-key map (kbd "C-'") nil)
    (define-key map (kbd "C-,") nil)
    (define-key map (kbd "C-c M-l") #'org-insert-last-stored-link)
    (define-key map (kbd "C-c C-M-l") #'org-toggle-link-display)
    (define-key map (kbd "C-c SPC") #'consult-org-heading)))

;;; Org extensions (lib-org.el)
(ryan-emacs-builtin-package 'lib-org
  (add-hook 'org-capture-after-finalize-hook #'ryan-org-capture-cleanup-frame-h)
  
  (setq org-agenda-format-date #'ryan-org-agenda-format-date-aligned)

  (define-key global-map (kbd "C-c A") (lambda () (interactive) (org-agenda nil "A"))))

;;; Task dependencies (org-edna)
(ryan-emacs-elpa-package 'org-edna
  (setq org-edna-use-inheritance t)
  (org-edna-mode 1))

;;; Stylized org files (org-modern.el)
(ryan-emacs-elpa-package 'org-modern
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

;;; Appointment reminders (appt.el)
(ryan-emacs-builtin-package 'appt
  (setq appt-display-diary nil)
  
  (run-at-time 10 nil #'appt-activate 1))

(provide 'init-org)

;;; init-org.el ends here
