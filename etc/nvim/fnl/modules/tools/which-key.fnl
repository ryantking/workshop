(import-macros {:def-augroup au- :let-g g-} "zest.macros")

(local {: setup : register} (require "which-key"))

(local M {})

(fn M.config []
  (let [margin (-> (nvim.fn.winwidth 0) (- 120) (/ 2) (math.max 0))]
    (setup
      {:plugins {:spelling true}
       :window {:border "shadow"
                :margin [1 margin 2 margin]}
       :layout {:align "center"}
       :ignore_missing true})
    (call "keymap.leader" :register {})))

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

(fn M.add-go-bindings [bufnr]
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
         :a {:name "Add"
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
