;;; config.el -*- lexical-binding: t; -*-

;; [[file:config.org::*Personal Information][Personal Information:1]]
(setq user-full-name "@gitName@"
      user-mail-address "@gitEmail@")
;; Personal Information:1 ends here

;; [[file:config.org::*Personal Information][Personal Information:2]]
(setq auth-sources '("~/.local/share/doom/authinfo.gpg")
      auth-source-cache-expiry nil)
;; Personal Information:2 ends here

;; [[file:config.org::*Better Defaults][Better Defaults:1]]
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-margin 2                             ; It's nice to maintain a little margin
      evil-vsplit-window-right t                  ; Vertical split to the right
      evil-split-window-below t)                  ; Split below

(global-subword-mode 1)                           ; Iterate through CamelCase words
;; Better Defaults:1 ends here

;; [[file:config.org::*Better Defaults][Better Defaults:2]]
(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))
;; Better Defaults:2 ends here

;; [[file:config.org::*Better Defaults][Better Defaults:3]]
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
;; Better Defaults:3 ends here

;; [[file:config.org::*UI Settings][UI Settings:1]]
(setq fringe-mode 0
      tool-bar-mode 0)
;; UI Settings:1 ends here

;; [[file:config.org::*Fonts][Fonts:1]]
(setq doom-font (font-spec :family "@monoFamily@" :weight '@monoWeight@ :size @monoSize@)
      doom-big-font (font-spec :family "@monoFamily@" :weight '@monoWeight@ :size @monoBigSize@)
      doom-unicode-font (font-spec :family "@monoUnicodeFamily@" :weight '@monoWeight@ :size @monoSize@)
      doom-variable-pitch-font (font-spec :family "@sansFamily@" :weight '@sansWeight@ :size @sansSize@)
      doom-serif-font (font-spec :family "@serifFamily@" :weight '@serifWeight@ :size @serifSize@))
;; Fonts:1 ends here

;; [[file:config.org::*Fonts][Fonts:2]]
(defvar mixed-pitch-modes '(org-mode LaTeX-mode markdown-mode gfm-mode Info-mode)
  "Modes to enable `mixed-pitch-mode' in, but only after UI initialisation.")

(defun init-mixed-pitch-h ()
  "Hook `mixed-pitch-mode' into each mode in `mixed-pitch-modes'.
Also immediately enables `mixed-pitch-modes' if currently in one of the modes."
  (when (memq major-mode mixed-pitch-modes)
    (mixed-pitch-mode 1))
  (dolist (hook mixed-pitch-modes)
    (add-hook (intern (concat (symbol-name hook) "-hook")) #'mixed-pitch-mode)))

(add-hook 'doom-init-ui-hook #'init-mixed-pitch-h)
;; Fonts:2 ends here

;; [[file:config.org::*Fonts][Fonts:3]]
(after! mixed-pitch
  (defface variable-pitch-serif
    '((t (:family "serif")))
    "A variable-pitch face with serifs."
    :group 'basic-faces)
  (setq mixed-pitch-set-height t)
  (setq variable-pitch-serif-font (font-spec :family "Alegreya" :size 27))
  (set-face-attribute 'variable-pitch-serif nil :font variable-pitch-serif-font)
  (defun mixed-pitch-serif-mode (&optional arg)
    "Change the default face of the current buffer to a serifed variable pitch, while keeping some faces fixed pitch."
    (interactive)
    (let ((mixed-pitch-face 'variable-pitch-serif))
      (mixed-pitch-mode (or arg 'toggle)))))
;; Fonts:3 ends here

;; [[file:config.org::*Themes][Themes:1]]
(setq doom-theme '@theme@)
(setq doom-nord-padded-modeline t)
;; Themes:1 ends here

;; [[file:config.org::*Dashboard][Dashboard:1]]
(map! :map +doom-dashboard-mode-map
      :ne "f" #'find-file
      :ne "r" #'consult-recent-file
      :ne "p" #'doom/open-private-config
      :ne "c" (cmd! (find-file (expand-file-name "config.org" doom-private-dir)))
      :ne "." (cmd! (doom-project-find-file "~/Workshop/"))
      :ne "b" #'+vertico/switch-workspace-buffer
      :ne "B" #'consult-buffer
      :ne "q" #'save-buffers-kill-terminal)
;; Dashboard:1 ends here

;; [[file:config.org::*Splash Screen][Splash Screen:1]]
(defvar fancy-splash-image-template
  (expand-file-name "emacs-e.svg" doom-private-dir)
  "Default template svg used for the splash image, with substitutions from ")

(defvar fancy-splash-sizes
  `((:height 300 :min-height 50 :padding (0 . 2))
    (:height 250 :min-height 42 :padding (2 . 4))
    (:height 200 :min-height 35 :padding (3 . 3))
    (:height 150 :min-height 28 :padding (3 . 3))
    (:height 100 :min-height 20 :padding (2 . 2))
    (:height 75  :min-height 15 :padding (2 . 1))
    (:height 50  :min-height 10 :padding (1 . 0))
    (:height 1   :min-height 0  :padding (0 . 0)))
  "list of plists with the following properties
  :height the height of the image
  :min-height minimum `frame-height' for image
  :padding `+doom-dashboard-banner-padding' (top . bottom) to apply
  :template non-default template file
  :file file to use instead of template")

(defvar fancy-splash-template-colours
  '(("$colour1" . keywords) ("$colour2" . type) ("$colour3" . base5) ("$colour4" . base8))
  "list of colour-replacement alists of the form (\"$placeholder\" . 'theme-colour) which applied the template")

(unless (file-exists-p (expand-file-name "theme-splashes" doom-cache-dir))
  (make-directory (expand-file-name "theme-splashes" doom-cache-dir) t))

(defun fancy-splash-filename (theme-name height)
  (expand-file-name (concat (file-name-as-directory "theme-splashes")
                            theme-name
                            "-" (number-to-string height) ".svg")
                    doom-cache-dir))

(defun fancy-splash-clear-cache ()
  "Delete all cached fancy splash images"
  (interactive)
  (delete-directory (expand-file-name "theme-splashes" doom-cache-dir) t)
  (message "Cache cleared!"))

(defun fancy-splash-generate-image (template height)
  "Read TEMPLATE and create an image if HEIGHT with colour substitutions as
   described by `fancy-splash-template-colours' for the current theme"
  (with-temp-buffer
    (insert-file-contents template)
    (re-search-forward "$height" nil t)
    (replace-match (number-to-string height) nil nil)
    (dolist (substitution fancy-splash-template-colours)
      (goto-char (point-min))
      (while (re-search-forward (car substitution) nil t)
        (replace-match (doom-color (cdr substitution)) nil nil)))
    (write-region nil nil
                  (fancy-splash-filename (symbol-name doom-theme) height) nil nil)))

(defun fancy-splash-generate-images ()
  "Perform `fancy-splash-generate-image' in bulk"
  (dolist (size fancy-splash-sizes)
    (unless (plist-get size :file)
      (fancy-splash-generate-image (or (plist-get size :template)
                                       fancy-splash-image-template)
                                   (plist-get size :height)))))

(defun ensure-theme-splash-images-exist (&optional height)
  (unless (file-exists-p (fancy-splash-filename
                          (symbol-name doom-theme)
                          (or height
                              (plist-get (car fancy-splash-sizes) :height))))
    (fancy-splash-generate-images)))

(defun get-appropriate-splash ()
  (let ((height (frame-height)))
    (cl-some (lambda (size) (when (>= height (plist-get size :min-height)) size))
             fancy-splash-sizes)))

(setq fancy-splash-last-size nil)
(setq fancy-splash-last-theme nil)
(defun set-appropriate-splash (&rest _)
  (let ((appropriate-image (get-appropriate-splash)))
    (unless (and (equal appropriate-image fancy-splash-last-size)
                 (equal doom-theme fancy-splash-last-theme)))
    (unless (plist-get appropriate-image :file)
      (ensure-theme-splash-images-exist (plist-get appropriate-image :height)))
    (setq fancy-splash-image
          (or (plist-get appropriate-image :file)
              (fancy-splash-filename (symbol-name doom-theme) (plist-get appropriate-image :height))))
    (setq +doom-dashboard-banner-padding (plist-get appropriate-image :padding))
    (setq fancy-splash-last-size appropriate-image)
    (setq fancy-splash-last-theme doom-theme)
    (+doom-dashboard-reload)))

(add-hook 'window-size-change-functions #'set-appropriate-splash)
(add-hook 'doom-load-theme-hook #'set-appropriate-splash)
;; Splash Screen:1 ends here

;; [[file:config.org::*Splash Screen][Splash Screen:2]]
(defvar splash-phrase-source-folder
  (expand-file-name "misc/splash-phrases" doom-private-dir)
  "A folder of text files with a fun phrase on each line.")

(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splase-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defvar splase-phrase--cache nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splase-phrase--cache))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splase-phrase--cache)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun doom-dashboard-phrase ()
  "Get a splash phrase, flow it over multiple lines as needed, and make fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defadvice! doom-dashboard-widget-loaded-with-phrase ()
  :override #'doom-dashboard-widget-loaded
  (setq line-spacing 0.2)
  (insert
   "\n\n"
   (propertize
    (+doom-dashboard--center
     +doom-dashboard--width
     (doom-display-benchmark-h 'return))
    'face 'doom-dashboard-loaded)
   "\n"
   (doom-dashboard-phrase)
   "\n"))
;; Splash Screen:2 ends here

;; [[file:config.org::*Splash Screen][Splash Screen:3]]
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
(add-hook! '+doom-dashboard-mode-hook (hide-mode-line-mode 1) (hl-line-mode -1))
(setq-hook! '+doom-dashboard-mode-hook evil-normal-state-cursor (list nil))
;; Splash Screen:3 ends here

;; [[file:config.org::*Splash Screen][Splash Screen:4]]
(defun doom-dashboard-draw-ascii-emacs-banner-fn ()
  (let* ((banner
          '(",---.,-.-.,---.,---.,---."
            "|---'| | |,---||    `---."
            "`---'` ' '`---^`---'`---'"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat
                 line (make-string (max 0 (- longest-line (length line)))
                                   32)))
               "\n"))
     'face 'doom-dashboard-banner)))

(unless (display-graphic-p) ; for some reason this messes up the graphical splash screen atm
  (setq +doom-dashboard-ascii-banner-fn #'doom-dashboard-draw-ascii-emacs-banner-fn))
;; Splash Screen:4 ends here

;; [[file:config.org::*Misc][Misc:1]]
(setq display-line-numbers-type 'relative)
;; Misc:1 ends here

;; [[file:config.org::*Misc][Misc:2]]
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")
;; Misc:2 ends here

;; [[file:config.org::*Keybindings][Keybindings:1]]
(map!
 "C-<left>" #'evil-window-left
 "C-<right>" #'evil-window-right
 "C-<up>" #'evil-window-up
 "C-<down>" #'evil-window-down

 "C-S-<left>" #'+evil/window-move-left
 "C-S-<right>" #'+evil/window-move-right
 "C-S-<up>" #'+evil/window-move-up
 "C-S-<down>" #'+evil/window-move-down)
;; Keybindings:1 ends here

;; [[file:config.org::*Asynchronous Configuration Tangling][Asynchronous Configuration Tangling:1]]
(defvar +literate-tangle--proc nil)
(defvar +literate-tangle--proc-start-time nil)

(defadvice! +literate-tangle-async-h ()
  "A very simplified version of `+literate-tangle-h', but async."
  :override #'+literate-tangle-h
  (unless (getenv "__NOTANGLE")
    (let ((default-directory doom-private-dir))
      (when +literate-tangle--proc
        (message "Killing outdated tangle process...")
        (set-process-sentinel +literate-tangle--proc #'ignore)
        (kill-process +literate-tangle--proc)
        (sit-for 0.3)) ; ensure the message is seen for a bit
      (setq +literate-tangle--proc-start-time (float-time)
            +literate-tangle--proc
            (start-process "tangle-config"
                           (get-buffer-create " *tangle config*")
                           "emacs" "--batch" "--eval"
                           (format "(progn \
(require 'ox) \
(require 'ob-tangle) \
(setq org-confirm-babel-evaluate nil \
      org-inhibit-startup t \
      org-mode-hook nil \
      write-file-functions nil \
      before-save-hook nil \
      after-save-hook nil \
      vc-handled-backends nil \
      org-startup-folded nil \
      org-startup-indented nil) \
(org-babel-tangle-file \"%s\" \"%s\"))"
                                   +literate-config-file
                                   (expand-file-name (concat doom-module-config-file ".el")))))
      (set-process-sentinel +literate-tangle--proc #'+literate-tangle--sentinel)
      (run-at-time nil nil (lambda () (message "Tangling config.org"))) ; ensure shown after a save message
      "Tangling config.org...")))

(defun +literate-tangle--sentinel (process signal)
  (cond
   ((and (eq 'exit (process-status process))
         (= 0 (process-exit-status process)))
    (message "Tangled config.org sucessfully (took %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))
   ((memq (process-status process) (list 'exit 'signal))
    (+popup-buffer (get-buffer " *tangle config*"))
    (message "Failed to tangle config.org (after %.1fs)"
             (- (float-time) +literate-tangle--proc-start-time))
    (setq +literate-tangle--proc nil))))

(defun +literate-tangle-check-finished ()
  (when (and (process-live-p +literate-tangle--proc)
             (yes-or-no-p "Config is currently retangling, would you please wait a few seconds?"))
    (switch-to-buffer " *tangle config*")
    (signal 'quit nil)))
(add-hook! 'kill-emacs-hook #'+literate-tangle-check-finished)
;; Asynchronous Configuration Tangling:1 ends here

;; [[file:config.org::*Native Compilation][Native Compilation:1]]
(when 'native-comp-compiler-options
  (setq native-comp-compiler-options '("-O3")))
;; Native Compilation:1 ends here

;; [[file:config.org::*Window Title][Window Title:1]]
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
;; Window Title:1 ends here

;; [[file:config.org::*Daemon][Daemon:1]]
(defun greedily-do-daemon-setup ()
  (require 'org)
  (require 'vertico)
  (require 'consult)
  (require 'marginalia)
  (when (require 'mu4e nil t)
    (setq mu4e-confirm-quit t)
    (setq +mu4e-lock-greedy t)
    (setq +mu4e-lock-relaxed t)
    (+mu4e-lock-add-watcher)
    (when (+mu4e-lock-available t)
      (mu4e~start))))

(when (daemonp)
  (add-hook 'emacs-startup-hook #'greedily-do-daemon-setup)
  (add-hook 'emacs-startup-hook #'init-mixed-pitch-h))
;; Daemon:1 ends here

;; [[file:config.org::*Company][Company:1]]
(after! company
   (setq company-idle-delay 0.1
      company-minimum-prefix-length 1
      company-selection-wrap-around t
      company-require-match 'never
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-other-buffers nil
      company-tooltip-limit 5
      company-tooltip-minimum-width 50))
(set-company-backend!
  '(text-mode
    markdown-mode
    gfm-mode)
  '(:seperate
    company-yasnippet
    company-ispell
    company-files))

;;nested snippets
(setq yas-triggers-in-field t)
;; Company:1 ends here

;; [[file:config.org::*Modeline][Modeline:1]]
(after! doom-modeline
  (setq doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-with-project
        doom-modeline-modal-icon t
        inhibit-compacting-font-caches t))
;; Modeline:1 ends here

;; [[file:config.org::*IBuffer][IBuffer:1]]
(setq-hook! 'ibuffer-hook ibuffer-formats
            '((mark modified read-only locked " "
                    (name 50 18 :left :elide)
                    " "
                    (size 9 -1 :right)
                    " "
                    (mode 16 16 :left :elide)
                    " " filename-and-process)
              (mark " "
                    (name 16 -1)
                    " " filename)))
;; IBuffer:1 ends here

;; [[file:config.org::*Term][Term:2]]
(setq explicit-shell-file-name (executable-find "fish"))
;; Term:2 ends here

;; [[file:config.org::*Term][Term:3]]
(setq +ligatures-in-modes t)
;; Term:3 ends here

;; [[file:config.org::*~Vterm~][~Vterm~:1]]
(setq vterm-always-compile-module t)
;; ~Vterm~:1 ends here

;; [[file:config.org::*~Vterm~][~Vterm~:2]]
(setq vterm-kill-buffer-on-exit t)
;; ~Vterm~:2 ends here

;; [[file:config.org::*Org][Org:1]]
(defun dropbox-file (path)
  (concat (getenv "HOME") "/Dropbox/" path))

(defun dropbox-files (&rest paths)
  (mapcar #'dropbox-file paths))
;; Org:1 ends here

;; [[file:config.org::*Org][Org:2]]
(setq org-directory (dropbox-file "org"))

(sp-local-pair
 '(org-mode)
 "<<" ">>"
 :actions '(insert))

(after! org
  (setq

   org-agenda-files
   (dropbox-files "org/inbox.org" "org/personal.org" "org/projects.org" "org/inbox_work.org" "org/work.org")

   org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence"BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)"
              "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)""CANC(k@)"))

   org-agenda-prefix-format
   '((agenda . " %i %?-12t% s")
     (todo . " %i")
     (tags . " %i %-12:c")
     (search . " %i %-12:c"))

   org-capture-templates
   `(("p" "Personal" entry (file ,(dropbox-file "org/inbox.org")) "* TODO %?\n %i\n %a")
     ("w" "Work" entry (file+headline ,(dropbox-file "org/inbox_work.org") "To refile") "* TODO %?\n %i\n %a"))))
;; Org:2 ends here

;; [[file:config.org::*Agenda][Agenda:1]]
(after! org-agenda
  (setq
   org-agenda-custom-commands
   '(("a" "Personal"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-files (dropbox-files "org/personal.org"))
                (org-deadline-warning-days 7)
                (org-agenda-overriding-header "Agenda\n")))
       (todo "TODO"
             ((org-agenda-overriding-header "To Refile\n")
              (org-agenda-prefix-format "  ")
              (org-agenda-files (dropbox-files "org/inbox.org"))))
       (todo "NEXT"
             ((org-agenda-overriding-header "Projects\n")
              (org-agenda-prefix-format "  %c (%e) | ")
              (org-agenda-files (dropbox-files "org/projects.org"))))))
     ("w" "Work"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-files (dropbox-files "org/work.org"))
                (org-deadline-warning-days 14)
                (org-agenda-overriding-header "Via\n")))
       (todo "TODO"
             ((org-agenda-overriding-header "To Refile\n")
              (org-agenda-prefix-format "  ")
              (org-agenda-files (dropbox-files "org/inbox_work.org"))))
       (todo "NEXT"
             ((org-agenda-overriding-header "Projects\n")
              (org-agenda-prefix-format "  %i %-12:c [%e] ")
              (org-agenda-files (dropbox-files "org/work.org"))))
       (todo "WAITING"
             ((org-agenda-overriding-header "Waiting on others\n")
              (org-agenda-files (dropbox-files "org/work.org")))))))))

;; Highlight additional keywords
(after! hl-todo
  (setq hl-todo-keyword-faces '(("NEXT" warning bold))))
;; Agenda:1 ends here

;; [[file:config.org::*Roam][Roam:1]]
(after! org-roam
  (setq org-roam-directory (dropbox-file "roam")))

(after! deft
  (setq deft-directory (dropbox-file "roam")))
;; Roam:1 ends here

;; [[file:config.org::*Journal][Journal:1]]
(after! org-journal
  (setq
   org-journal-date-prefix "#+title: "
   org-journal-file-format "%Y-%m-%d.org"
   org-journal-dir (dropbox-file "roam")
   org-journal-date-format "%Y-%m-%d"))
;; Journal:1 ends here

;; [[file:config.org::*To File][To File:1]]
;; Appearance
(use-package frame
  :config
  (setq-default default-frame-alist
                (append (list
                         '(internal-border-width . 24)
                         '(left-fringe    . 0)
                         '(right-fringe   . 0)
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         ;; '(line-spacing . 0.35)
                         '(vertical-scroll-bars . nil))))
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)
  :custom
  (window-divider-default-right-width 24)
  (window-divider-default-bottom-width 12)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))

(custom-set-faces!
  `(vertical-border :background ,(doom-color 'bg) :foreground ,(doom-color 'bg)))

(when (boundp 'window-divider-mode)
  (setq window-divider-default-places nil
        window-divider-default-bottom-width 0
        window-divider-default-right-width 0)
  (window-divider-mode -1))

(defadvice! fix-+evil-default-cursor-fn ()
  :override #'+evil-default-cursor-fn
  (evil-set-cursor-color (face-background 'cursor)))

(defadvice! fix-+evil-emacs-cursor-fn ()
  :override #'+evil-emacs-cursor-fn
  (evil-set-cursor-color (face-foreground 'warning)))

(add-hook 'before-make-frame-hook 'window-divider-mode)

;; remove encoding indicator
(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (and (memq (plist-get (coding-system-plist buffer-file-coding-system) :category)
                                 '(coding-category-undecided coding-category-utf-8))
                           (not (memq (coding-system-eol-type buffer-file-coding-system) '(1 2))))
                t)))
(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding) ;;remove encoding

;; Evil Mode
(setq evil-snipe-scope 'visible
      evil-want-fine-undo t
      evil-vsplit-window-right t
      evil-split-window-below t)


;; Vertico
(after! marginalia
  (setq marginalia-censor-variables nil)

  (defadvice! +marginalia--anotate-local-file-colorful (cand)
    "Just a more colourful version of `marginalia--anotate-local-file'."
    :override #'marginalia--annotate-local-file
    (when-let (attrs (file-attributes (substitute-in-file-name
                                       (marginalia--full-candidate cand))
                                      'integer))
      (marginalia--fields
       ((marginalia--file-owner attrs)
        :width 12 :face 'marginalia-file-owner)
       ((marginalia--file-modes attrs))
       ((+marginalia-file-size-colorful (file-attribute-size attrs))
        :width 7)
       ((+marginalia--time-colorful (file-attribute-modification-time attrs))
        :width 12))))

  (defun +marginalia--time-colorful (time)
    (let* ((seconds (float-time (time-subtract (current-time) time)))
           (color (doom-blend
                   (face-attribute 'marginalia-date :foreground nil t)
                   (face-attribute 'marginalia-documentation :foreground nil t)
                   (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
      ;; 1 - log(3 + 1/(days + 1)) % grey
      (propertize (marginalia--time time) 'face (list :foreground color))))

  (defun +marginalia-file-size-colorful (size)
    (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
           (color (if (< size-index 10000000) ; 10m
                      (doom-blend 'orange 'green size-index)
                    (doom-blend 'red 'orange (- size-index 1)))))
      (propertize (file-size-human-readable size) 'face (list :foreground color)))))

;; Treemacs
(setq treemacs-width 40)
(setq doom-themes-treemacs-theme "doom-colors")

;; Tramp
(setq tramp-default-remote-shell "/run/current-system/sw/bin/dash"
      projectile-mode-line "Projectile")
;; To File:1 ends here
