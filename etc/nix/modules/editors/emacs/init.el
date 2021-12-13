;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +childframe)
       (vertico +icons)

       :ui
       deft
       doom
       doom-dashboard
       doom-quit
       (:if IS-LINUX (emoji +unicode +github))
       ;; fill-column
       hl-todo
       ligatures
       modeline
       nav-flash
       ophints
       (popup +all +defaults)
       (treemacs +lsp)
       vc-gutter
       window-select
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       snippets
       word-wrap

       :emacs
       (dired +ranger +icons)
       electric
       (ibuffer +icons)
       (undo +tree)
       vc

       :term
       eshell
       vterm

       :checkers
       syntax
       (spell +aspell)
       grammar

       :tools
       (debugger +lsp)
       direnv
       (docker +lsp)
       (eval +overlay)
       gist
       (lookup +devdocs +docsets)
       (lsp +peek)
       (magit +forge)
       make
       pdf
       rgb
       taskrunner
       tmux
       upload

       :os
       (:if IS-MAC macos)
       (tty +osc)

       :lang
       data
       emacs-lisp
       (go +lsp)
       (json +lsp)
       (lua +fennel)
       markdown
       nix
       (org +hugo +journal +pretty +roam2)
       (rust +lsp)
       (sh +fish +lsp)
       (yaml +lsp)

       :config
       literate
       (default +bindings +smartparens))
