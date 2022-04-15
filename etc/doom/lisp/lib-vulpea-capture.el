;;; lib-vulpea-capture.el --- Capturing tasks and notes -*- lexical-binding: t; -*-
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
;; Various utilities for capturing tasks and notes.
;;
;;; Code:

(require 'vulpea)
(require 'org)
(require 'org-capture)
(require 'org-roam)

(defconst env-graphic-p (display-graphic-p))
(defconst env-rootp (string-equal "root" (getenv "USER")))
(defconst env-sys-mac-p (eq system-type 'darwin))
(defconst env-sys-linux-p (eq system-type 'gnu/linux))
(defconst env-sys-name (system-name))

(defvar vulpea-capture-inbox-file
  (format "inbox-%s.org" env-sys-name)
  "The path to the inbox file.
It is relative to `vulpea-directory', unless it is absolute.")

(defun vulpea-capture-select-template-prettier (&optional keys)
  "Select a capture template, in a prettier way than default
Lisp programs can force the template by setting KEYS to a string."
  (let ((org-capture-templates
         (or (org-contextualize-keys
              (org-capture-upgrade-templates org-capture-templates)
              org-capture-templates-contexts)
             '(("t" "Task" entry (file+headline "" "Tasks")
                "* TODO %?\n  %u\n  %a")))))
    (if keys
        (or (assoc keys org-capture-templates)
            (error "No capture template referred to by \"%s\" keys" keys))
      (org-mks org-capture-templates
               "Select a capture template\n━━━━━━━━━━━━━━━━━━━━━━━━━"
               "Template key: "
               `(("q" ,(concat (all-the-icons-octicon "stop" :face 'all-the-icons-red :v-adjust 0.01) "\tAbort")))))))


(defun vulpea-mks-pretty (table title &optional prompt specials)
  "Select a member of an alist with multiple keys. Prettified.

TABLE is the alist which should contain entries where the car is a string.
There should be two types of entries.

1. prefix descriptions like (\"a\" \"Description\")
   This indicates that `a' is a prefix key for multi-letter selection, and
   that there are entries following with keys like \"ab\", \"ax\"…

2. Select-able members must have more than two elements, with the first
   being the string of keys that lead to selecting it, and the second a
   short description string of the item.

The command will then make a temporary buffer listing all entries
that can be selected with a single key, and all the single key
prefixes.  When you press the key for a single-letter entry, it is selected.
When you press a prefix key, the commands (and maybe further prefixes)
under this key will be shown and offered for selection.

TITLE will be placed over the selection in the temporary buffer,
PROMPT will be used when prompting for a key.  SPECIALS is an
alist with (\"key\" \"description\") entries.  When one of these
is selected, only the bare key is returned."
  (save-window-excursion
    (let ((inhibit-quit t)
          (buffer (org-switch-to-buffer-other-window "*Org Select*"))
          (prompt (or prompt "Select: "))
          case-fold-search
          current)
      (unwind-protect
          (catch 'exit
            (while t
              (setq-local evil-normal-state-cursor (list nil))
              (erase-buffer)
              (insert title "\n\n")
              (let ((des-keys nil)
                    (allowed-keys '("\C-g"))
                    (tab-alternatives '("\s" "\t" "\r"))
                    (cursor-type nil))
                ;; Populate allowed keys and descriptions keys
                ;; available with CURRENT selector.
                (let ((re (format "\\`%s\\(.\\)\\'"
                                  (if current (regexp-quote current) "")))
                      (prefix (if current (concat current " ") "")))
                  (dolist (entry table)
                    (pcase entry
                      ;; Description.
                      (`(,(and key (pred (string-match re))) ,desc)
                       (let ((k (match-string 1 key)))
                         (push k des-keys)
                         ;; Keys ending in tab, space or RET are equivalent.
                         (if (member k tab-alternatives)
                             (push "\t" allowed-keys)
                           (push k allowed-keys))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) (propertize "›" 'face 'font-lock-comment-face) "  " desc "…" "\n")))
                      ;; Usable entry.
                      (`(,(and key (pred (string-match re))) ,desc . ,_)
                       (let ((k (match-string 1 key)))
                         (insert (propertize prefix 'face 'font-lock-comment-face) (propertize k 'face 'bold) "   " desc "\n")
                         (push k allowed-keys)))
                      (_ nil))))
                ;; Insert special entries, if any.
                (when specials
                  (insert "─────────────────────────\n")
                  (pcase-dolist (`(,key ,description) specials)
                    (insert (format "%s   %s\n" (propertize key 'face '(bold all-the-icons-red)) description))
                    (push key allowed-keys)))
                ;; Display UI and let user select an entry or
                ;; a sub-level prefix.
                (goto-char (point-min))
                (unless (pos-visible-in-window-p (point-max))
                  (org-fit-window-to-buffer))
                (let ((pressed (org--mks-read-key allowed-keys
                                                  prompt
                                                  (not (pos-visible-in-window-p (1- (point-max)))))))
                  (setq current (concat current pressed))
                  (cond
                   ((equal pressed "\C-g") (user-error "Abort"))
                   ;; Selection is a prefix: open a new menu.
                   ((member pressed des-keys))
                   ;; Selection matches an association: return it.
                   ((let ((entry (assoc current table)))
                      (and entry (throw 'exit entry))))
                   ;; Selection matches a special entry: return the
                   ;; selection prefix.
                   ((assoc current specials) (throw 'exit current))
                   (t (error "No entry available")))))))
        (when buffer (kill-buffer buffer))))))

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

;;;###autoload
(defun vulpea-capture-task ()
  "Capture a task."
  (interactive)
  (org-capture nil "t"))

(defun vulpea-capture-meeting ()
  "Capture a meeting."
  (interactive)
  (org-capture nil "m"))

(defun vulpea-capture-meeting-target ()
  "Return a target for a meeting capture."
  (let ((person (org-capture-get :meeting-person)))
    ;; unfortunately, I could not find a way to reuse
    ;; `org-capture-set-target-location'
    (if (vulpea-note-id person)
        (let ((path (vulpea-note-path person))
              (headline "Meetings"))
          (set-buffer (org-capture-target-buffer path))
          ;; Org expects the target file to be in Org mode, otherwise
          ;; it throws an error. However, the default notes files
          ;; should work out of the box. In this case, we switch it to
          ;; Org mode.
          (unless (derived-mode-p 'org-mode)
            (org-display-warning
             (format
              "Capture requirement: switching buffer %S to Org mode"
              (current-buffer)))
            (org-mode))
          (org-capture-put-target-region-and-position)
          (widen)
          (goto-char (point-min))
          (print (re-search-forward
               (format org-complex-heading-regexp-format
                       (regexp-quote headline))
               nil t))
          (if (re-search-forward
               (format org-complex-heading-regexp-format
                       (regexp-quote headline))
               nil t)
              (beginning-of-line)
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "* " headline "\n")
            (beginning-of-line 0)))
      (let ((path vulpea-capture-inbox-file))
        (set-buffer (org-capture-target-buffer path))
        (org-capture-put-target-region-and-position)
        (widen)))))

(defun vulpea-capture-meeting-template ()
  "Return a template for a meeting capture."
  (let ((person (vulpea-select
                 "Person"
                 :filter-fn
                 (lambda (note)
                   (let ((tags (vulpea-note-tags note)))
                     (seq-contains-p tags "people"))))))
    (org-capture-put :meeting-person person)
    (if (vulpea-note-id person)
        "* MEETING [%<%Y-%m-%d %a>] :REFILE:MEETING:\n%U\n\n%?"
      (concat "* MEETING with "
              (vulpea-note-title person)
              " on [%<%Y-%m-%d %a>] :MEETING:\n%U\n\n%?"))))

(defvar vulpea-capture-person-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "people/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n"
            "\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for person.
Variables in the capture context are provided by
`vulpea-create'.")

(defvar vulpea-capture-article-template
  `("d" "default" plain
    #'org-roam-capture--get-point
    "%?"
    :file-name "litnotes/%<%Y%m%d%H%M%S>-${slug}"
    :head ,(concat
            ":PROPERTIES:\n"
            ":ID:                     ${id}\n"
            ":END:\n"
            "#+TITLE: ${title}\n"
            "#+ROAM_KEY: ${url}\n"
            "#+ROAM_TAGS: Content:Article Status:New\n"
            "\n")
    :unnarrowed t
    :immediate-finish t)
  "Capture template for article.
Variables in the capture context are provided by
`vulpea-capture-article'.")

;;;###autoload
(defun vulpea-capture-article ()
  "Capture an article using `vulpea-capture-article-template'.
User is asked to provide an URL, title and authors of the article
being captured.
Title is inferred from URL, but user may edit it.
Authors can be created on the fly. See
`vulpea-capture-person-template' for more information."
  (interactive)
  (when-let*
      ((url (read-string "URL: "))
       (title (org-cliplink-retrieve-title-synchronously url))
       (title (read-string "Title: " title))
       (people (fun-collect-while
                (lambda ()
                  (let ((person
                         (vulpea-select
                          "Person"
                          :filter-fn
                          (lambda (note)
                            (let ((tags (vulpea-note-tags note)))
                              (seq-contains-p tags "people"))))))
                    (if (vulpea-note-id person)
                        person
                      (vulpea-create
                       (vulpea-note-title person)
                       vulpea-capture-person-template))))
                nil))
       (note (vulpea-create title
                            vulpea-capture-article-template
                            (list (cons 'url url)))))
    (vulpea-utils-with-note note
      (vulpea-meta-set note "authors" people t))
    (find-file (vulpea-note-path note))))

(defun vulpea-capture-journal ()
  "Capture a journal entry.
By default it uses current date to find a daily. With
\\[universal-argument] user may select the date."
  (interactive)
  (cond
   ((equal current-prefix-arg '(4))     ; select date
    (org-roam-dailies-capture-date))
   (t
    (org-roam-dailies-capture-today))))

(provide 'lib-vulpea-capture)
;;; lib-vulpea-capture.el ends here
