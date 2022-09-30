;;; vftc-dired.el --- VFTC Emacs Dired Extensions -*- lexical-binding: t -*-

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file defines a few faces that mirror the Nano faces.
;; They use doom theme colors to make faces that change with the theme.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'vftc-common)

(defgroup vftc-dired ()
  "Extensions for Dired."
  :group 'dired)

;;;; File associations

(defcustom vftc-dired-media-extensions
  "\\.\\(mp[34]\\|ogg\\|flac\\|webm\\|mkv\\)"
  "Regular expression for media file extensions.

Also see the function `vftc-dired-media-player' and the variable
`vftc-dired-media-players'.

To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type 'string
  :group 'vftc-dired)

(defcustom vftc-dired-image-extensions
  "\\.\\(png\\|jpe?g\\|tiff\\)"
  "Regular expression for media file extensions.

Also see the function `vftc-dired-image-viewer' and the variable
`vftc-dired-image-viewers'.

To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type 'string
  :group 'vftc-dired)

(defcustom vftc-dired-media-players '("mpv" "vlc")
  "List of strings for media player programs.

Also see the function `vftc-dired-media-player' and the variable
`vftc-dired-media-extensions'.

To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type '(repeat string)
  :group 'vftc-dired)

(defcustom vftc-dired-image-viewers '("feh" "sxiv")
  "List of strings for image viewer programs.

Also see the function `vftc-dired-image-viewer' and the variable
`vftc-dired-image-extensions'.

To be used in user configurations while setting up the variable
`dired-guess-shell-alist-user'."
  :type '(repeat string)
  :group 'vftc-dired)

;; NOTE 2021-06-28: I am not sure why the compiler complains without
;; this, even though we require cl-lib.
(declare-function cl-remove-if "cl-lib")

(defmacro vftc-dired-file-association (name programs)
  "Make NAME function to check for PROGRAMS."
  (declare (indent defun))
  `(defun ,name ()
     ,(format "Return available program.

This checks each entry in `%s' and returns the first program that
is available on the system.  If none is present, it falls back to
xdg-open (for GNU/Linux only).

This function is for use in `dired-guess-shell-alist-user'."
              programs)
     (catch :found
       (dolist (p (append ,programs '("xdg-open")))
         (when (executable-find p)
           (throw :found p))))))

(vftc-dired-file-association
 vftc-dired-media-player
 vftc-dired-media-players)

(vftc-dired-file-association
 vftc-dired-image-viewer
 vftc-dired-image-viewers)

;;;; General commands

(autoload 'dired-mark-files-regexp "dired")
(autoload 'dired-toggle-marks "dired")
(autoload 'dired-do-kill-lines "dired-aux")

(defvar vftc-dired--limit-hist '()
  "Minibuffer history for `vftc-dired-limit-regexp'.")

;;;###autoload
(defun vftc-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'vftc-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines)
  (add-to-history 'vftc-dired--limit-hist regexp))

(defvar vftc-dired--find-grep-hist '()
  "Minibuffer history for `vftc-dired-grep-marked-files'.")

;; Also see `vftc-search-grep' from vftc-search.el.
;;;###autoload
(defun vftc-dired-grep-marked-files (regexp &optional arg)
  "Run `find' with `grep' for REGEXP on marked files.
When no files are marked or when just a single one is marked,
search the entire directory instead.

With optional prefix ARG target a single marked file.

We assume that there is no point in marking a single file and
running find+grep on its contents.  Visit it and call `occur' or
run grep directly on it without the whole find part."
  (interactive
   (list
    (read-string "grep for PATTERN (marked files OR current directory): " nil 'vftc-dired--find-grep-hist)
    current-prefix-arg)
   dired-mode)
  (when-let* ((marks (dired-get-marked-files 'no-dir))
              (files (mapconcat #'identity marks " "))
              (args (if (or arg (length> marks 1))
                        ;; Thanks to Sean Whitton for pointing out an
                        ;; earlier superfluity of mine: we do not need
                        ;; to call grep through find when we already
                        ;; know the files we want to search in.  Check
                        ;; Sean's dotfiles:
                        ;; <https://git.spwhitton.name/dotfiles>.
                        ;;
                        ;; Any other errors or omissions are my own.
                        (format "grep -nH --color=auto %s %s" (shell-quote-argument regexp) files)
                      (concat
                       "find . -not " (shell-quote-argument "(")
                       " -wholename " (shell-quote-argument "*/.git*")
                       " -prune " (shell-quote-argument ")")
                       " -type f"
                       " -exec grep -nHE --color=auto " regexp " "
                       (shell-quote-argument "{}")
                       " " (shell-quote-argument ";") " "))))
    (compilation-start
     args
     'grep-mode
     (lambda (mode) (format "*vftc-dired-find-%s for '%s'" mode regexp))
     t)))

;;;; Subdir extras and Imenu setup

(defvar vftc-dired--directory-header-regexp "^ +\\(.+\\):\n"
  "Pattern to match Dired directory headings.")

;;;###autoload
(defun vftc-dired-subdirectory-next (&optional arg)
  "Move to next or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir vftc-dired--directory-header-regexp))
    (goto-char (point-at-eol))
    (if (re-search-forward subdir nil t (or arg nil))
        (progn
          (goto-char (match-beginning 1))
          (goto-char (point-at-bol)))
      (goto-char pos))))

;;;###autoload
(defun vftc-dired-subdirectory-previous (&optional arg)
  "Move to previous or optional ARGth Dired subdirectory heading.
For more on such headings, read `dired-maybe-insert-subdir'."
  (interactive "p")
  (let ((pos (point))
        (subdir vftc-dired--directory-header-regexp))
    (goto-char (point-at-bol))
    (if (re-search-backward subdir nil t (or arg nil))
        (goto-char (point-at-bol))
      (goto-char pos))))

(autoload 'dired-current-directory "dired")
(autoload 'dired-kill-subdir "dired-aux")

;;;###autoload
(defun vftc-dired-remove-inserted-subdirs ()
  "Remove all inserted Dired subdirectories."
  (interactive)
  (goto-char (point-max))
  (while (and (vftc-dired-subdirectory-previous)
              (not (equal (dired-current-directory)
                          (expand-file-name default-directory))))
    (dired-kill-subdir)))

(autoload 'cl-remove-if-not "cl-seq")

(defun vftc-dired--dir-list (list)
  "Filter out non-directory file paths in LIST."
  (cl-remove-if-not
   (lambda (dir)
     (file-directory-p dir))
   list))

(defun vftc-dired--insert-dir (dir &optional flags)
  "Insert DIR using optional FLAGS."
  (dired-maybe-insert-subdir (expand-file-name dir) (or flags nil)))

(autoload 'dired-get-filename "dired")
(autoload 'dired-get-marked-files "dired")
(autoload 'dired-maybe-insert-subdir "dired-aux")
(defvar dired-subdir-switches)
(defvar dired-actual-switches)

;;;###autoload
(defun vftc-dired-insert-subdir (&optional arg)
  "Generic command to insert subdirectories in Dired buffers.

When items are marked, insert those which are subsirectories of
the current directory.  Ignore regular files.

If no marks are active and point is on a subdirectory line,
insert it directly.

If no marks are active and point is not on a subdirectory line,
prompt for a subdirectory using completion.

With optional ARG as a single prefix (`\\[universal-argument]')
argument, prompt for command line flags to pass to the underlying
'ls' program.

With optional ARG as a double prefix argument, remove all
inserted subdirectories."
  (interactive "p")
  (let* ((name (dired-get-marked-files))
         (flags (when (eq arg 4)
                  (read-string "Flags for `ls' listing: "
                               (or dired-subdir-switches dired-actual-switches)))))
    (cond  ; NOTE 2021-07-20: `length>', `length=' are from Emacs28
     ((eq arg 16)
      (vftc-dired-remove-inserted-subdirs))
     ((and (length> name 1) (vftc-dired--dir-list name))
      (mapc (lambda (file)
              (when (file-directory-p file)
                (vftc-dired--insert-dir file flags)))
            name))
     ((and (length= name 1) (file-directory-p (car name)))
      (vftc-dired--insert-dir (car name) flags))
     (t
      (let ((selection (read-directory-name "Insert directory: ")))
        (vftc-dired--insert-dir selection flags))))))

(defun vftc-dired--imenu-prev-index-position ()
  "Find the previous file in the buffer."
  (let ((subdir vftc-dired--directory-header-regexp))
    (re-search-backward subdir nil t)))

(defun vftc-dired--imenu-extract-index-name ()
  "Return the name of the file at point."
  (file-relative-name
   (buffer-substring-no-properties (+ (point-at-bol) 2)
                                   (1- (point-at-eol)))))

;;;###autoload
(defun vftc-dired-setup-imenu ()
  "Configure imenu for the current dired buffer.
Add this to `dired-mode-hook'."
  (set (make-local-variable 'imenu-prev-index-position-function)
       'vftc-dired--imenu-prev-index-position)
  (set (make-local-variable 'imenu-extract-index-name-function)
       'vftc-dired--imenu-extract-index-name))

(provide 'vftc-dired)

;;; vftc-dired.el ends here
