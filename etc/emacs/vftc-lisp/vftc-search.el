;;; vftc-search.el --- VFTC Emacs Search Utilities -*- lexical-binding: t -*-

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

;; Some utility functions that make the builtin search work slightly better.

;;; Code:

(require 'isearch)
(require 'replace)
(require 'grep)
(require 'vftc-common)

(defgroup vftc-search ()
  "Setup for Isearch, Occur, and related."
  :group 'search)

(defcustom vftc-search-outline-regexp-alist
  '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
    (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)"))
  "Alist of regular expressions per major mode.

For best results the key must be a symbol that corresponds to a
major mode.

To be used by `vftc-search-occur-outline'."
  :type 'alist
  :group 'vftc-search)

(defcustom vftc-search-todo-keywords
  "TODO\\|FIXME\\|NOTE\\|REVIEW\\|BUG"
  "Regexp with search to-do keywords."
  :type 'string
  :group 'vftc-search)

;;
;;; isearch

;;;###autoload
(defun vftc-search-isearch-other-end ()
  "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
  (interactive)
  (isearch-done)
  (when isearch-other-end
    (goto-char isearch-other-end)))

;;;###autoload
(defun vftc-search-isearch-abort-dwim ()
  "Delete failed `isearch' input, single char, or cancel search.

This is a modified variant of `isearch-abort' that allows us to
perform the following, based on the specifics of the case: (i)
delete the entirety of a non-matching part, when present; (ii)
delete a single character, when possible; (iii) exit current
search if no character is present and go back to point where the
search started."
  (interactive)
  (if (eq (length isearch-string) 0)
      (isearch-cancel)
    (isearch-del-char)
    (while (or (not isearch-success) isearch-error)
      (isearch-pop-state)))
  (isearch-update))

;;;###autoload
(defun vftc-search-isearch-repeat-forward (&optional arg)
  "Move forward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-forward (or arg 1)))

;;;###autoload
(defun vftc-search-isearch-repeat-backward (&optional arg)
  "Move backward, keeping point at the beginning of the match.
Optionally move to ARGth match in the given direction."
  (interactive "p")
  (when (and (not isearch-forward) isearch-other-end)
    (goto-char isearch-other-end))
  (isearch-repeat-backward (or arg 1)))

(defmacro vftc-search-isearch-occurrence (name edge &optional doc)
  "Construct function for moving to `isearch' occurrence.
NAME is the name of the function.  EDGE is either the beginning
or the end of the buffer.  Optional DOC is the resulting
function's docstring."
  `(defun ,name (&optional arg)
     ,doc
     (interactive "p")
     (let ((x (or arg 1))
           (command (intern (format "isearch-%s-of-buffer" ,edge))))
       (isearch-forward-symbol-at-point)
       (funcall command x))))

(vftc-search-isearch-occurrence
 vftc-search-isearch-beginning-of-buffer
 "beginning"
 "Run `isearch-beginning-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
beginning of the buffer.")

(vftc-search-isearch-occurrence
 vftc-search-isearch-end-of-buffer
 "end"
 "Run `isearch-end-of-buffer' for the symbol at point.
With numeric ARG, move to ARGth occurrence counting from the
end of the buffer.")

(defvar vftc-search-markup-replacements
  '((elisp-to-org-code "`\\(.*?\\)'" "~\\1~")
    (elisp-to-org-verbatim "`\\(.*?\\)'" "=\\1=")
    (org-to-elisp-quote "[=~]\\(.*?\\)[=~]" "`\\1'")
    (org-to-markdown-code "[=~]\\(.*?\\)[=~]" "`\\1`"))
  "Common markup replacement patterns.")

(defvar vftc-search--replace-markup-history '()
  "Minibuffer history of `vftc-search-replace-markup'.")

(defun vftc-search--replace-markup-prompt ()
  "Prompt for font set (used by `fontaine-set-fonts')."
  (let* ((def (nth 0 vftc-search--replace-markup-history))
         (prompt (if def
                     (format "Replace markup TYPE [%s]: " def)
                   "Replace markup TYPE: ")))
    (intern
     (completing-read
      prompt
      ;; TODO 2022-05-01: maybe older Emacs versions need to explicitly
      ;; map through the car of each list?
      vftc-search-markup-replacements
      nil t nil 'vftc-search--replace-markup-history def))))

(defun vftc-search-replace-markup (type)
  "Perform TYPE of markup replacement.
TYPE is the car of a list in `vftc-search-markup-replacements'.

When used interactively, prompt for completion among the
available types.

When the region is active, only perform replacements within its
boundaries, else start from point to the end of the buffer."
  (interactive (list (vftc-search--replace-markup-prompt)))
  (if-let* ((types vftc-search-markup-replacements)
            ((memq type (mapcar #'car types)))
            (association (alist-get type types))
            (search (nth 0 association))
            (replace (nth 1 association)))
      (if (use-region-p)
          (replace-regexp-in-region search replace (region-beginning) (region-end))
        (while (re-search-forward search nil t)
          (replace-match replace)))
    (user-error "`%s' is not part of `vftc-search-markup-replacements'" type)))

;; TODO: make this work backwardly when given a negative argument
(defun vftc-search-isearch-replace-symbol ()
  "Run `query-replace-regexp' for the symbol at point."
  (interactive)
  (isearch-forward-symbol-at-point)
  (isearch-query-replace-regexp))

(autoload 'goto-address-mode "goto-addr")

;;;###autoload
(defun vftc-search-occur-urls ()
  "Produce buttonised list of all URLs in the current buffer."
  (interactive)
  (let ((buf-name (format "*links in <%s>*" (buffer-name))))
    (add-hook 'occur-hook #'goto-address-mode)
    (occur-1 vftc-common-url-regexp "\\&" (list (current-buffer)) buf-name)
    (remove-hook 'occur-hook #'goto-address-mode)))

;;;###autoload
(defun vftc-search-occur-browse-url ()
  "Point browser at a URL in the buffer using completion.
Which web browser to use depends on the value of the variable
`browse-url-browser-function'.

Also see `vftc-search-occur-urls'."
  (interactive)
  (let ((matches nil))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp vftc-common-url-regexp nil t)
        (push (match-string-no-properties 0) matches)))
    (funcall browse-url-browser-function
             (completing-read "Browse URL: " matches nil t))))

(defvar vftc-search--occur-outline-hist '()
  "Minibuffer history of `vftc-search-occur-outline'.")

(defun vftc-search--occur-outline-prompt ()
  "Helper prompt for `vftc-search-occur-outline'."
  (let* ((alist vftc-search-outline-regexp-alist)
         (key (car (assoc major-mode alist)))
         (default (or key (nth 1 vftc-search--occur-outline-hist))))
    (completing-read
     (format "Outline style [%s]: " default)
     (mapcar #'car alist)
     nil nil nil 'vftc-search--occur-outline-hist default)))

(defvar-local vftc-search--remap-cookie nil
  "Current local value of `vftc-search--remap-match-face'.")

(defface vftc-search-match '((t :inherit default))
  "Face intended to override `match' buffer-locally.")

(defun vftc-search--remap-match-face (buf)
  "Remap `match' to `vftc-search-match' in BUF."
  (with-current-buffer buf
    (setq vftc-search--remap-cookie
          (face-remap-add-relative 'match 'vftc-search-match))))

;;;###autoload
(defun vftc-search-occur-outline (&optional arg)
  "Produce buffer outline from `vftc-search-outline-regexp-alist'.

With optional prefix ARG (\\[universal-argument]), prompt for a
preset among the entries in `vftc-search-outline-regexp-alist'.

ARG may also be a string (or regular expression) when called from
Lisp."
  (interactive "P")
  (let* ((regexp (when (and arg (not (stringp arg)))
                   (vftc-search--occur-outline-prompt)))
         (rx (cond
              ((stringp arg)
               arg)
              ((and arg (string= major-mode regexp))
               (alist-get regexp vftc-search-outline-regexp-alist))
              ((assoc major-mode vftc-search-outline-regexp-alist)
               (alist-get major-mode vftc-search-outline-regexp-alist))
              (t (user-error "Unknown outline style"))))
         (buf-name (format "*outline of <%s>*" (buffer-name))))
    (occur-1 rx nil (list (current-buffer)) buf-name)
    ;; Because we are producing an outline, we do not need to know what
    ;; the exact matches are.
    (vftc-search--remap-match-face buf-name)
    (add-to-history 'vftc-search--occur-outline-hist regexp)))

;;;###autoload
(defun vftc-search-occur-todo-keywords (&optional context)
  "Produce Occur buffer with `vftc-search-todo-keywords'.
With optional numeric prefix argument for CONTEXT, show as many
lines before and after each match.

When called from Lisp CONTEXT must satisfy `natnump'.  A faulty
value is read as 0.

Also see `vftc-search-grep-todo-keywords'."
  (interactive "P")
  (let* ((case-fold-search nil)
         (num (cond
               (current-prefix-arg
	            (prefix-numeric-value current-prefix-arg))
               (t (if (natnump context) context 0))))
         (buf-name (format "*keywords in <%s>*" (buffer-name))))
    (occur-1 vftc-search-todo-keywords num (list (current-buffer)) buf-name)))

;;;; Grep

(defvar vftc-search--grep-hist '()
  "Input history of grep searches.")

;;;###autoload
(defun vftc-search-grep (regexp &optional recursive)
  "Run grep for REGEXP.

Search in the current directory using `lgrep'.  With optional
prefix argument (\\[universal-argument]) for RECURSIVE, run a
search starting from the current directory with `rgrep'."
  (interactive
   (list
    (read-from-minibuffer (concat (if current-prefix-arg
                                      (propertize "Recursive" 'face 'warning)
                                    "Local")
                                  " grep for PATTERN: ")
                          nil nil nil 'vftc-search--grep-hist)
    current-prefix-arg))
  (unless grep-command
    (grep-compute-defaults))
  (if recursive
      (rgrep regexp "*" default-directory)
    (lgrep regexp "*" default-directory)
    (add-to-history 'vftc-search--grep-hist regexp)))

;;;###autoload
(defun vftc-search-grep-todo-keywords (&optional arg)
  "Use `vftc-search-grep' to find `vftc-search-todo-keywords'.

With optional prefix ARG use git-grep instead for the entire
repository (runs `vftc-search-git-grep-todo-keywords').  If Git
is not available on the system, run `vftc-search-grep'
recursively, starting from the current directory.

Also see `vftc-search-occur-todo-keywords'."
  (interactive "P")
  (cond
   (arg
    (if (executable-find "git")
        (vftc-search-git-grep-todo-keywords)
      (vftc-search-grep vftc-search-todo-keywords t)))
   (t
    (vftc-search-grep vftc-search-todo-keywords))))

;; NOTE 2022-01-30: We could use `project-find-regexp' but I prefer
;; grep's editable buffers.  Besides, where is the fun in that when we
;; can use `compilation-start' instead?
;;;###autoload
(defun vftc-search-git-grep-todo-keywords ()
  "Use the git-grep mechanism for `vftc-search-todo-keywords'."
  (interactive)
  (let ((regexp vftc-search-todo-keywords)
        (default-directory (or (vc-root-dir)
                               (locate-dominating-file "." ".git")
                               default-directory)))
    (compilation-start
     (format "git --no-pager grep -n --color=auto -r -I -E -e %s" regexp)
     'grep-mode
     (lambda (mode) (format "*vftc-search-git-%s for '%s'" mode regexp))
     t)))

(provide 'vftc-search)

;;; vftc-search.el ends here
