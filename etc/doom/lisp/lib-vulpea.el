;;; lib-vulpea.el --- Utilities for Vulpea -*- lexical-binding: t; -*-
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
;; This module provides various utilities for working with Vulpea.
;;
;;; Code:

(defun vulpea-buffer-p ()
  "Return non-nil if the current buffer buffer is a note."
  (and buffer-file-name
       (eq major-mode 'org-mode)
       (string-suffix-p "org" buffer-file-name)
       (string-prefix-p
        (expand-file-name (file-name-as-directory vulpea-directory))
        (file-name-directory buffer-file-name))))

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.
Ignores TODO entries marked as done, which means that this
function returns nil if current buffer contains only completed
tasks. The only exception is headings tagged as REFILE."
  (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
    (lambda (h)
      (or (eq 'todo (org-element-property :todo-type h))
          (seq-contains-p (org-element-property :tags h)
                          "REFILE")))
    nil 'first-match))

;;;###autoload
(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))

;;;###autoload
(defun vulpea-insert-handle (note)
  "Hook to be called on NOTE after `vulpea-insert'."
  (when-let* ((title (vulpea-note-title note))
              (tags (vulpea-note-tags note)))
    (when (seq-contains-p tags "people")
      (save-excursion
        (ignore-errors
          (org-back-to-heading)
          (when (eq 'todo (org-element-property :todo-type (org-element-at-point)))
            (org-set-tags
             (seq-uniq
              (cons
               (vulpea--title-to-tag title)
               (org-get-tags nil t))))))))))

;;;###autoload
(defun vulpea-tags-add ()
  "Add a tag to current note."
  (interactive)
  (org-with-point-at 1
    (when (call-interactively #'org-roam-tag-add)
      (vulpea-ensure-filetag))))

;;;###autoload
(defun vulpea-tags-delete ()
  "Delete a tag from current note."
  (interactive)
  (call-interactively #'org-roam-tag-remove))

(defun vulpea-ensure-filetag ()
  "Add missing FILETAGS to the current note."
  (require 'vulpea)
  (let* ((file (buffer-file-name))
         (path-tags
          (when file
            (seq-filter
             (lambda (x) (not (string-empty-p x)))
             (split-string
              (string-remove-prefix
               vulpea-directory
               (file-name-directory file))
              "/"))))
         (original-tags (vulpea-buffer-tags-get))
         (tags (append original-tags path-tags)))

    ;; process people
    (when (seq-contains-p tags "people")
      (let ((tag (vulpea--title-as-tag)))
        (unless (seq-contains-p tags tag)
          (setq tags (cons tag tags)))))

    ;; process litnotes
    ;; (setq tags (litnotes-ensure-filetags tags))

    ;; process projects
    (if (vulpea-project-p)
        (setq tags (cons "project" tags))
      (setq tags (remove "project" tags)))

    (setq tags (seq-uniq tags))

    ;; update tags if changed
    (when (or (seq-difference tags original-tags)
              (seq-difference original-tags tags))
      (apply #'vulpea-buffer-tags-set (seq-uniq tags)))))

;;;###autoload
(defun vulpea-alias-add ()
  "Add an alias to current note."
  (interactive)
  (call-interactively #'org-roam-alias-add))

;;;###autoload
(defun vulpea-alias-delete ()
  "Delete an alias from current note."
  (interactive)
  (call-interactively #'org-roam-alias-remove))

;;;###autoload
(defun vulpea-alias-extract ()
  "Extract an alias from current note as a separate note.
Make all the links to this alias point to newly created note."
  (interactive)
  (if-let* ((node (org-roam-node-at-point 'assert))
            (aliases (org-roam-node-aliases node)))
      (let* ((alias (completing-read
                     "Alias: " aliases nil 'require-match))
             (backlinks (seq-map
                         #'org-roam-backlink-source-node
                         (org-roam-backlinks-get node)))
             (id-old (org-roam-node-id node)))
        (org-roam-alias-remove alias)
        (org-roam-db-update-file (org-roam-node-file node))
        (let* ((note (vulpea-create
                      alias
                      "%<%Y%m%d%H%M%S>-${slug}.org"
                      :immediate-finish t
                      :unnarrowed t)))
          (seq-each
           (lambda (node)
             (vulpea-utils-with-file (org-roam-node-file node)
               (goto-char (point-min))
               (let ((link-old
                      (org-link-make-string
                       (concat "id:" id-old)
                       alias))
                     (link-new
                      (vulpea-utils-link-make-string note)))
                 (while (search-forward link-old nil 'noerror)
                   (replace-match link-new))))
             (org-roam-db-update-file (org-roam-node-file node)))
           backlinks)))
    (user-error "No aliases to extract")))

;;;###autoload
(defun vulpea-setup-buffer (&optional _)
  "Setup current buffer for notes viewing and editing."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (org-with-point-at 1
      (org-hide-drawer-toggle 'off))
    (setq-local tab-width 1)
    (vulpea-ensure-filetag)))

;;;###autoload
(defun vulpea-pre-save-hook ()
  "Do all the dirty stuff when file is being saved."
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (vulpea-ensure-filetag)))

;;;###autoload
(defun vulpea-db-build ()
  "Update notes database."
  (when (file-directory-p vulpea-directory)
    (org-roam-db-sync)
    (org-roam-update-org-id-locations)))

(defun directory-subdirs (directory &optional rec)
  "Return subdirs or files of DIRECTORY.
If REC is non-nil then do recursive search."
  (let ((res
         (seq-map
          #'file-name-as-directory
          (seq-remove
           (lambda (file)
             (or (string-match "\\`\\."
                               (file-name-nondirectory file))
                 (string-match "\\`#.*#\\'"
                               (file-name-nondirectory file))
                 (string-match "~\\'"
                               (file-name-nondirectory file))
                 (not (file-directory-p file))))
           (directory-files directory t)))))
    (if rec
        (apply
         #'append
         (seq-map (lambda (p) (cons p (directory-subdirs p)))
                  res))
      res)))

;;;###autoload
(defun vulpea-subdir-select ()
  "Select notes subdirectory."
  (interactive)
  (let ((dirs (cons
               "."
               (seq-map
                (lambda (p)
                  (string-remove-prefix vulpea-directory p))
                (directory-subdirs vulpea-directory 'recursive)))))
    (completing-read "Subdir: " dirs nil t)))

(defun vulpea--title-as-tag ()
  "Return title of the current note as tag."
  (vulpea--title-to-tag (vulpea-buffer-prop-get "title")))

(defun vulpea--title-to-tag (title)
  "Convert TITLE to tag."
  (concat "@"
    (s-replace-all '((" " . "") ("'" . "")) title)))

;;;###autoload
(defun vulpea-activate-link (start end path _brackets)
  "Activate a link between START and END for PATH."
  (let ((visible-start (or (match-beginning 3)
                           (match-beginning 2)))
        (visible-end (or (match-end 3) (match-end 2))))
    (when-let* ((uuid-p (string-match-p string-uuid-regexp path))
                (note (vulpea-db-get-by-id path))
                (tags (vulpea-note-tags note))
                (icon (cond
                       ((seq-contains-p tags litnotes-tag)
                        (litnotes-content-display
                         (litnotes-entry-content
                          (litnotes-entry note))
                         :height 0.8 :v-adjust 0.04))
                       ((seq-contains-p tags "people")
                        (concat
                         (all-the-icons-material
                          "person" :height 0.8 :v-adjust 0.04)
                         "\t"))
                       ((seq-contains-p tags "grape")
                        (concat "🍇" "\t"))
                       ((seq-contains-p tags "cellar")
                        (concat "🍷" "\t"))
                       ((seq-contains-p tags "aroma")
                        (concat
                         (all-the-icons-material
                          "local_florist" :height 0.8 :v-adjust 0.04)
                         "\t"))))
                (desc (buffer-substring visible-start visible-end))
                (desc (concat icon desc))
                (desc (s-truncate (- end start) desc))
                (l (length desc))
                (hidden `(invisible
                          ,(or (org-link-get-parameter "id" :display)
                               'org-link))))
      (remove-text-properties start visible-start hidden)
      (remove-text-properties visible-end end hidden)
      (add-text-properties (+ start l) end hidden)
      (put-text-property start end 'display desc))))

(provide 'lib-vulpea)
;;; lib-vulpea.el ends here
