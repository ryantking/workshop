(import-macros {:def-augroup au-
                :def-keymap ki-
                :def-autocmd-fn fau-} "zest.macros")

(local {: setup : register} (require "which-key"))


(local M {})

(fn M.config []
  (let [{: setup : register} (require "which-key")
        margin (-> (nvim.fn.winwidth 0) (- 120) (/ 2) (math.max 0))]
    (setup
      {:plugins {:spelling true}
       :window {:border "shadow"
                :margin [1 margin 2 margin]}
       :layout {:align "center"}
       :ignore_missing true})
    (register
      {:w ["<cmd>w!<CR>" "Save"]
       :q ["<cmd>q!<CR>" "Quit"]
       :y ["\"+y" "Yank to clipboard"]
       :Y ["\"+yy" "Yank line to clipboard"]
       :p ["<Plug>(miniyank-startput)" "Put most recent item after"]
       :P ["<Plug>(miniyank-startPut)" "Put most recent item before"]
       :n ["<Plug>(miniyank-cycle)" "Cycle history"]
       :N ["<Plug>(miniyank-cycleback)" "Cycle history backwards"]
       :h [":nohlsearch<CR>" "No Highlight"]
       :f ["<cmd>Telescope find_files<CR>" "Find File"]
       :e ["<cmd>NvimTreeToggle<CR>" "Explorer"]
       :F ["<cmd>NvimTreeFindFile<CR>" "Find file in explorer"]
       :t ["<cmd>TableModeToggle<CR>" "Toggle Table Mode"]
       ; :p {:name "+packer"
       ;     :s ["<cmd>PackerSync<CR>" "Sync"]
       ;     :S ["<cmd>PackerStatus<CR>" "Status"]
       ;     :c ["<cmd>PackerCompile<CR>" "Compile"]
       ;     :p ["<cmd>PackerProfile<CR>" "Profile"]}
       :b {:name "+buffer"
           :c ["<cmd>BufferClose!<CR>" "Close Buffer"]
           :n ["<cmd>BufferNext<CR>" "Next Buffer"]
           :N ["<cmd>BufferPrev<CR>" "Previous Buffer"]
           :j ["<cmd>BufferPick<CR>" "Jump to Buffer"]
           :f ["<cmd>Telescope buffers<CR>" "Find Buffer"]
           :w ["<cmd>BufferWipeout<CR>" "Wipeout Buffer"]
           :e ["<cmd>BufferCloseAllButCurrent<CR>" "Close all buffers except current"]
           "[" ["<cmd>BufferCloseBuffersLeft<CR>" "Close all buffers to the left"]
           "]" ["<cmd>BufferCloseBuffersRight<CR>" "Close all buffers to the right"]
           :d ["<cmd>BufferOrderByDirectory<CR>" "Sort bufferline by directory"]
           :L ["<cmd>BufferOrderByLanguage<CR>" "Sort bufferline by language"]}
       :s {:name "+search"
           :p ["<cmd>Telescope projects<CR>" "Find Project"]
           :g ["<cmd>Telescope live_grep<CR>" "Grep"]
           :b ["<cmd>Telescope current_buffer_fuzzy_find<CR>" "Buffer"]
           :h ["<cmd>Telescope help_tags<cr>" "Help Tags"]
           :m ["<cmd>Telescope man_pages<cr>" "Man Pages"]
           :f ["<cmd>Telescope frecency<cr>" "Recent Files"]
           :r ["<cmd>Telescope registers<cr>" "Registers"]
           :c ["<cmd>Telescope commands<cr>" "Commands"]}}
      {:prefix "<leader>"})))

(fn M.add-git-bindings []
  (register
    {:g {:name "+git"
         :g ["<cmd>Neogit<CR>" "Neogit"]
         :f ["<cmd>Telescope git_status<CR>" "Open Changed File"]
         :b ["<cmd>Telescope git_branches<CR>" "Checkout Branch"]
         :c ["<cmd>Telescope git_commits<CR>" "Checkout Commit"]
         :C ["<cmd>Telescope git_bcommits<CR>" "Checkout Commit (for current file)"]
         :w ["<cmd>Telescope git_worktree git_worktrees<CR>" "Switch worktree"]
         :W ["<cmd>Telescope git_worktree create_git_worktree<CR>" "Create worktree"]
         :b ["<cmd>lua require'gitsigns'.blame_line()<CR>" "Blame"]
         :r ["<cmd>lua require'gitsigns'.reset_buffer()<CR>" "Rest Buffer"]
         :h {:name "+hunk"
             :n ["<cmd>lua require'gitsigns'.next_hunk()<CR>" "Next"]
             :N ["<cmd>lua require'gitsigns'.prev_hunk()<CR>" "Prev"]
             :r ["<cmd>lua require'gitsigns'.reset_hunk()<CR>" "Reset"]
             :s ["<cmd>lua require'gitsigns'.stage_hunk()<CR>" "Stage"]
             :u ["<cmd>lua require'gitsigns'.undo_stage_hunk()<CR>" "Undo Stage"]
             :p ["<cmd>lua require'gitsigns'.preview_hunk()<CR>" "Preview"]}
         :d {:name "Diff"
             :d ["<cmd>DiffviewOpen<CR>" "Current Changes"]
             :m [(.. "<cmd>DiffviewOpen " (master) "<CR>") "Against master"]
             :l ["<cmd>DiffviewOpen HEAD~1<CR>" "Against the last commit"]
             :r ["<cmd>DiffviewRefresh<CR>" "Refresh"]
             :c ["<cmd>DiffviewClose<CR>" "Close"]}}}
    {:prefix "<leader>"}))

(fn M.add-lsp-bindings [bufnr]
  (print "heret/")

  (register
    {:l {:name "+lsp"
         :a ["<cmd>lua vim.lsp.buf.code_action()<CR>" "Code Action"]
         :f ["<cmd>lua vim.lsp.buf.formatting()<CR>" "Format"]
         :i ["<cmd>LspInfo<CR>" "Info"]
         :r ["<cmd>lua vim.lsp.buf.rename()<CR>" "Rename"]
         :d {:name "+diagnostics"
             "]" ["<cmd>lua vim.lsp.diagnostic.goto_next({popop_opts = {border = 'single'}})<CR>" "Next"]
             "[" ["<cmd>lua vim.lsp.diagnostic.goto_prev({popop_opts = {border = 'single'}})<CR>" "Prev"]
             :d ["<cmd>TroubleToggle lsp_document_diagnostics<CR>" "Document"]
             :D ["<cmd>TroubleToggle lsp_document_diagnostics<CR>" "Workspace"]
             :s ["<cmd>Telescope lsp_document_diagnostics<CR>" "Search Document"]
             :S ["<cmd>Telescope lsp_workspace_diagnostics<CR>" "Search Workspace"]
             :t ["<cmd>TroubleToggle<CR>" "Toggle Trouble"]
             :q ["<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>" "Quickfix"]}
         :s {:name "+symbols"
             :s ["<cmd>Telescope lsp_document_symbols<CR>" "Document"]
             :S ["<cmd>Telescope lsp_dynamic_workspace_symbols<CR>" "Workspace"]
             :w ["<cmd>Trouble lsp_workspace_diagnostics<CR>" "Workspace Diagnostics"]
             :W ["<cmd>Telescope lsp_workspace_diagnostics<CR>" "Search Workspace Diagnostics"]
             }
         }}
    {:prefix "<leader>"})
  (register
    {:g {:r "References"
         :s "Signature help"
         :W "Workpace symbol"
         :D "Declaration"
         :0 "Doument symbol"
         :d "Goto definition"
         :T "Treesettir symbol"
         :i "Implementation"
         :L "Line diagnostics"
         :G "Diagnostics for all buffers"}}
    {:mode "n" :buffer bufnr}))

M
