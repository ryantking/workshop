(import-macros {:let-g g-
                :opt-get se?
                :def-autocmd au-
                :def-autocmd-fn fau-} "zest.macros")

(local loader (. (require "packer") :loader))

(fn syntax? []
  (not (is-ft "NvimTree" "guihua" "TelescopePrompt" "csv" "txt" "defx")))

(fn load-plugins []
  (loader "plenary.nvim")
  (g- vimsyn_embed "lPr")
  (loader "indent-blankline.nvim neoscroll.nvim nvim-scrollview")
  (when (is-git)
    (loader "gitsigns.nvim neogit git-worktree.nvim")
    (call "modules.tools.which-key" :add-git-bindings))
  (match (se? ft)
    "fennel" (loader "fennel.vim")
    "nix" (loader "vim-nix")))

(fn load-all-plugins []
  (load-plugins)
  (loader "nvim-lspconfig null-ls.nvim guihua.lua navigator.lua lsp_signature.nvim trouble.nvim")
  (match (se? ft)
    "lua" (loader "lua-dev.nvim")
    "go" (loader "go.nvim")
    "gomod" (loader "go.nvim"))
  (cmd "packadd nvim-treesitter")
  (loader "nvim-treesitter-textobjects nvim-treesitter-refactor nvim-ts-context-commentstring")
  (cmd "setlocal syntax=on"))

(fn syntax-autocmds []
  (au- "FileType" "guihua" "setlocal syntax=on")
  (fau- "FileType" "*" (cmd (.. "setlocal syntax=" (if (= (fclass) fclass-small) "on" "off")))))

(fn lazy-load []
  (cmd (.. "syntax " (if (syntax?) "manual" "on")))
  (match (fclass)
    fclass-small (load-all-plugins)
    fclass-medium (load-plugins))
  (syntax-autocmds))

(fau- "User" "LoadLazyPlugin" (lazy-load))

(vim.defer_fn #(nvim.command "doautocmd User LoadLazyPlugin") 50)
