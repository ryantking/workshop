(local {: register} (require "which-key"))

(local M {})

(fn M.register []
  (register
    {:. ["<cmd>Telescope file_browser<CR>" "Browse Files"]
     :/ ["<cmd>Telescope live_grep<CR>" "Search"]
     :: ["<cmd>Telescope command_history<CR>" "Command History"]
     :t ["<cmd>TableModeToggle<CR>" "Toggle Table Mode"]

     ;; Yank
     :y ["\"+y" "Yank to clipboard"]
     :Y ["\"+yy" "Yank line to clipboard"]
     :p ["<Plug>(miniyank-startput)" "Put most recent item after"]
     :P ["<Plug>(miniyank-startPut)" "Put most recent item before"]
     :n ["<Plug>(miniyank-cycle)" "Cycle history"]
     :N ["<Plug>(miniyank-cycleback)" "Cycle history backwards"]

     :f {:name "+file"
         :n ["<cmd>enew<CR>" "New"]
         :f ["<cmd>Telescope find_files<CR>" "Find File"]
         :t ["<cmd>NvimTreeToggle<CR>" "Tree"]
         :T ["<cmd>NvimTreeFindFile<CR>" "Current file in tree"]}

     :b {:name "+buffers"
         :c ["<cmd>BufferClose!<CR>" "Close"]
         :f ["<cmd>Telescope buffers<CR>" "Find Buffer"]
         :j ["<cmd>BufferPick<CR>" "Jump to Buffer"]
         :w ["<cmd>BufferWipeout<CR>" "Wipeout Buffer"]
         :e ["<cmd>BufferCloseAllButCurrent<CR>" "Close all buffers except current"]
         "[" ["<cmd>BufferCloseBuffersLeft<CR>" "Close all buffers to the left"]
         "]" ["<cmd>BufferCloseBuffersRight<CR>" "Close all buffers to the right"]
         :d ["<cmd>BufferOrderByDirectory<CR>" "Sort bufferline by directory"]
         :L ["<cmd>BufferOrderByLanguage<CR>" "Sort bufferline by language"]}

     :w {:name "+windows"
         :w ["<C-w>p" "Other"]
         :c ["<C-w>c" "Close"]
         :- ["<C-w>s" "Split below"]
         :| ["<C-w>v" "Split right"]
         :<Left> [":vertical resize +5<CR>" "Expand left (5)"]
         :<Right> [":vertical resize -5<CR>" "Expand right (5)"]
         :<Up> [":resize -5<CR>" "Expand up (5)"]
         :<Down> [":resize +5<CR>" "Expand down (5)"]
         :<S-Left> [":vertical resize +10<CR>" "Expand left (10)"]
         :<S-Right> [":vertical resize -10<CR>" "Expand right (10)"]
         :<S-Up> [":resize -10<CR>" "Expand up (10)"]
         :<S-Down> [":resize +10<CR>" "Expand down (10)"]
         := ["<C-w>=" "Balance"]}

     :<tab> {:name "+workspace"
             :<tab> ["<cmd>tabnew<CR>" "New Tab"]
             :n ["<cmd>tabnext<CR>" "Next"]
             :p ["<cmd>tabprevious<CR>" "Previous"]
             :f ["<cmd>tabfirst<CR>" "First"]
             :l ["<cmd>tablast<CR>" "Last"]}

     :s {:name "+search"
         :g ["<cmd>Telescope live_grep<CR>" "Grep"]
         :b ["<cmd>Telescope current_buffer_fuzzy_find<CR>" "Buffer"]
         :m ["<cmd>Telescope marks<CR>" "Marks"]
         :c ["<cmd>Telescope command_history<CR>" "Command History"]
         ; TODO(ryantking): Spectre
         }

     :t {:name "+toggle"
         :n [#(toggle "number relativenumber") "Line Number"]
         :s [#(toggle "spell") "Spell"]
         :w [#(toggle "wrap") "Wrap"]}

     :x {:name "+errors"
         :l ["<cmd>lopen<CR>" "Location List"]
         :q ["<cmd>copen<CR>" "Quickfix List"]}

     :h {:name "+help"
         :t ["<cmd>Telescope builtins<CR>" "Telescope builtins"]
         :c ["<cmd>Telescope commands<CR>" "Commands"]
         :h ["<cmd>Telescope help_tags<CR>" "Help Pages"]
         :m ["<cmd>Telescope man_pages<CR>" "Man Pages"]
         :k ["<cmd>Telescope keymaps<CR>" "Key Maps"]
         :h ["<cmd>Telescope highlights<CR>" "Highlight Groups"]
         :H ["<cmd>TSHighlightCapturesUnderCursor<CR>" "Highlight groups under cursor"]
         :o ["<cmd>Telescope options<CR>" "Options"]
         :a ["<cmd>Telescope autocommands<CR>" "Auto Commands"]
         :p {:name "+packer"
             :s ["<cmd>PackerSync<CR>" "Sync"]
             :S ["<cmd>PackerStatus<CR>" "Status"]
             :c ["<cmd>PackerCompile<CR>" "Compile"]
             :p ["<cmd>PackerProfile<CR>" "Profile"]}}}
    {:prefix "<leader>"}))

(fn M.register-git []
  (register
    {:g {:name "+git"
         :g ["<cmd>Neogit<CR>" "Neogit"]
         :f ["<cmd>Telescope git_status<CR>" "Status"]
         :b ["<cmd>Telescope git_branches<CR>" "Branches"]
         :c ["<cmd>Telescope git_commits<CR>" "Commits"]
         :C ["<cmd>Telescope git_bcommits<CR>" "Commits (for current file)"]
         :w ["<cmd>Telescope git_worktree git_worktrees<CR>" "Worktree"]
         :W ["<cmd>Telescope git_worktree create_git_worktree<CR>" "Create Worktree"]
         :b ["<cmd>lua require'gitsigns'.blame_line()<CR>" "Blame"]
         :r ["<cmd>lua require'gitsigns'.reset_buffer()<CR>" "Rest Buffer"]

         :h {:name "+hunk"
             :n ["<cmd>lua require'gitsigns'.next_hunk()<CR>" "Next"]
             :N ["<cmd>lua require'gitsigns'.prev_hunk()<CR>" "Prev"]
             :r ["<cmd>lua require'gitsigns'.reset_hunk()<CR>" "Reset"]
             :s ["<cmd>lua require'gitsigns'.stage_hunk()<CR>" "Stage"]
             :u ["<cmd>lua require'gitsigns'.undo_stage_hunk()<CR>" "Undo Stage"]
             :p ["<cmd>lua require'gitsigns'.preview_hunk()<CR>" "Preview"]}

         :d {:name "+diff"
             :d ["<cmd>DiffviewOpen<CR>" "DiffView"]
             :m [(.. "<cmd>DiffviewOpen " (master) "<CR>") "Against Master"]
             :l ["<cmd>DiffviewOpen HEAD~1<CR>" "Against the last commit"]
             :r ["<cmd>DiffviewRefresh<CR>" "Refresh"]
             :c ["<cmd>DiffviewClose<CR>" "Close"]}}}
    {:prefix "<leader>"}))

(fn M.register-lsp [bufnr]
  (register
    {:c {:name "+code"
         :c ["<cmd>lua vim.lsp.buf.code_action()<CR>" "Action"]
         :f ["<cmd>lua vim.lsp.buf.formatting()<CR>" "Format"]
         :r ["<cmd>lua vim.lsp.buf.rename()<CR>" "Rename"]}

     :l {:name "+lsp"
         :s ["<cmd>LspStart<CR>" "Start"]
         :S ["<cmd>LspStop<CR>" "Stop"]
         :i ["<cmd>LspInfo<CR>" "Info"]
         :r ["<cmd>LspRestart<CR>" "Restart"]}

     :s {:s ["<cmd>Telescope lsp_document_symbols<CR>" "Document Symbols"]
         :S ["<cmd>Telescope lsp_dynamic_workspace_symbols<CR>" "Workspace Symbols"]
         :d ["<cmd>Telescope lsp_document_diagnostics<CR>" "Document Diagnostics"]
         :D ["<cmd>Telescope lsp_workspace_diagnostics<CR>" "Workspace Diagnostics"]}

     :x {:x ["<cmd>TroubleToggle<CR>" "Trouble"]
         :d ["<cmd>TroubleToggle lsp_document_diagnostics<CR>" "Document"]
         :w ["<cmd>TroubleToggle lsp_workspace_diagnostics<CR>" "Workspace"]}}
    {:prefix "<leader>" :buffer bufnr}))

(fn M.register-go [bufnr]
  (register
    {:r ["<cmd>GoRun<CR>" "Run"]
     :b ["<cmd>GoBuild<CR>" "Build"]
     :g ["<cmd>GoGenerate<CR>" "Generate"]
     :l ["<cmd>GoLint<CR>" "Lint"]
     :f ["<cmd>Gofmt" "Format"]
     :i ["<cmd>Goimport" "Imports"]
     :m ["<cmd>GoMake<CR>" "Make"]

     :t {:name "+test"
         :t ["<cmd>GoTest<CR>" "All"]
         :T ["<cmd>GoCoverage<CR>" "All with Coverage"]
         :f ["<cmd>GoTestFunc<CR>" "Function"]
         :F ["<cmd>GoTestFile<CR>" "File"]

         :a {:name "+add"
             :t ["<cmd>GoAddTest<CR>" "Add Test"]
             :e ["<cmd>GoAddExpTest<CR>" "Add tests for exported functions"]
             :a ["<cmd>GoAddAllTest<CR>" "Add tests for all functions"]}}

     :T {:name "+tags"
         :a ["<cmd>GoAddTag<CR>" "Add"]
         :r ["<cmd>GoRmTag<CR>" "Remove"]
         :c ["<cmd>GoClearTag<CR>" "Clear"]}

     :F {:name "+fill"
         :s ["<cmd>GoFillStruct<CR>" "Fill Struct"]
         :S ["<cmd>GoFillSwitch<CR>" "Fill Switch"]
         :i ["<cmd>GoIfErr<CR>" "Fill if err"]}}
    {:prefix "<localleader>" :mode "n" :buffer bufnr}))

M
