(module plugins.which-key {require-macros [core.macros zest.macros]})

(local which-key {})

(fn which-key.config []
  (let [wk (require :which-key)]
    (fn _G.set_local_bindings []
      (let [bindings (. {:rust {:h {:name "Hints"
                                    :e ["<cmd>R stSetInlayHints<CR>" "Enable"]
                                    :d ["<cmd>RustSetInlayHints<CR>" "Disabile"]
                                    :t ["<cmd>RustSetInlayHints<CR>" "Toggle"]}
                                :r ["<cmd>RustRunnables<CR>" "Run"]
                                :d ["<cmd>RustDebugables<CR>" "Debug"]
                                :e ["<cmd>RustExpandMacro<CR>" "Expand Macro"]
                                :c ["<cmd>RustOpenCargo<CR>" "Open Cargo"]
                                :p ["<cmd>RustParentModule<CR>" "Parent Module"]
                                :j ["<cmd>RustJoinLines<CR>" "Join Lines"]
                                :a ["<cmd>RustHoverActions<CR>" "Code Actions"]
                                :h ["<cmd>RustHoverRange <CR>" "Hover Range"]
                                :m {:name "Move Item"
                                    :j ["<cmd>RustMoveItemDown<CR>" "Move Item Down"]
                                    :k ["<cmd>RustMoveItemUp<CR>" "Move Item Up"]}
                                :s ["<cmd>RustStartStandaloneServerForBuffer<CR>" "Start standalone LSP server"]}
                         :go {:r ["<cmd>GoRun<CR>" "Run"]
                              :b ["<cmd>GoBuild<CR>" "Build"]
                              :c ["<cmd>GoCoverage<CR>" "Test with Coverage"]
                              :g ["<cmd>GoGenerate<CR>" "Generate"]
                              :l ["<cmd>GoLint<CR>" "Lint"]
                              :f ["<cmd>Gofmt" "Format"]
                              :i ["<cmd>Goimport" "Imports"]
                              :m ["<cmd>GoMake<CR>" "Make"]
                              :t {:name "Test"
                                  :t ["<cmd>GoTest<CR>" "All"]
                                  :T ["<cmd>GoCoverage<CR>" "All wigh Coverage"]
                                  :f ["<cmd>GoTestFunc<CR>" "Function"]
                                  :F ["<cmd>GoTestFile<CR>" "File"]
                                  :a {:name "Add"
                                      :t ["<cmd>GoAddTest<CR>" "Add Test"]
                                      :e ["<cmd>GoAddExpTest<CR>" "Add tests for exported functions"]
                                      :a ["<cmd>GoAddAllTest<CR>" "Add tests for all functions"]}}
                              :T {:name "Tags"
                                  :a ["<cmd>GoAddTag<CR>" "Add"]
                                  :r ["<cmd>GoRmTag<CR>" "Remove"]
                                  :c ["<cmd>GoClearTag<CR>" "Clear"]}
                              :F {:name "Fill"
                                  :s ["<cmd>GoFillStruct<CR>" "Fill Struct"]
                                  :S ["<cmd>GoFillSwitch<CR>" "Fill Switch"]
                                  :i ["<cmd>GoIfErr<CR>" "Fill if err"]}
                              }
                         :markdown {:p ["<cmd>Glow<CR>" "Preview"]}}
                        (vim.api.nvim_buf_get_option 0 "filetype"))]
        (when bindings
          (wk.register bindings {:prefix "<localleader>" :buffer 0}))))

    (wk.setup {:window {:border :single
                        :margin [1 120 2 120]}
               :layout {:align :center}})
    (wk.register {:w ["<cmd>w!<CR>" "Save"]
                  :q ["<cmd>q!<CR>" "Quit"]
                  :/ ["<cmd>CommentToggle<CR>" "Comment"]
                  :c ["<cmd>BufferClose!<CR>" "Close Buffer"] 
                  :e ["<cmd>NvimTreeToggle<CR>" "Explorer"]
                  :f ["<cmd>Telescope find_files<CR>" "Find File"]
                  :p ["<cmd>Telescope projects<CR>" "Find Project"]
                  :h ["<cmd>nohlsearch<CR>" "No Highlight"] 
                  :b {:name "Buffers"
                      :n ["<cmd>BufferNext<CR>" "Next Buffer"]
                      :p ["<cmd>BufferNext<CR>" "Next Buffer"]
                      :j ["<cmd>BufferPick<CR>" "Jump to Buffer"]
                      :f ["<cmd>Telescope buffers<CR>" "Find Buffer"]
                      :w ["<cmd>BufferWipeout<CR>" "Wipeout Buffer"]
                      :e ["<cmd>BufferCloseAllButCurrent<CR>" "Close all buffers except current"]
                      :h ["<cmd>BufferCloseBuffersLeft<CR>" "Close all buffers to the left"]
                      :l ["<cmd>BufferCloseBuffersRight<CR>" "Close all buffers to the right"]
                      :D ["<cmd>BufferOrderByDirectory<CR>" "Sort bufferline by directory"]
                      :L ["<cmd>BufferOrderByLanguage<CR>" "Sort bufferline by language"]}
                  :P {:name "Packer"
                      :c ["<cmd>PackerCompile<CR>" "Compile"]
                      :i ["<cmd>PackerInstall<CR>" "Install"]
                      :s ["<cmd>PackerSync<CR>" "Sync"]
                      :S ["<cmd>PackerStatus<CR>" "Status"]
                      :u ["<cmd>PackerUpdate<CR>" "Update"]}
                  :g {:name "Git"
                      :g ["<cmd>Neogit<CR>" "Neogit"]
                      :n ["<cmd>lua require'gitsigns'.next_hunk()<CR>" "Next Hunk"]
                      :p ["<cmd>lua require'gitsigns'.prev_hunk()<CR>" "Prev Hunk"]
                      :l ["<cmd>lua require'gitsigns'.blame_line()<CR>" "Blame"]
                      :R ["<cmd>lua require'gitsigns'.preview_hunk()<CR>" "Preview Hunk"]
                      :r ["<cmd>lua require'gitsigns'.reset_hunk()<CR>" "Reset Hunk"]
                      :R ["<cmd>lua require'gitsigns'.reset_buffer()<CR>" "Rest Buffer"]
                      :s ["<cmd>lua require'gitsigns'.stage_hunk()<CR>" "Stage Hunk"]
                      :u ["<cmd>lua require'gitsigns'.undo_stage_hunk()<CR>" "Undo Stage Hunk"]
                      :o ["<cmd>Telescope git_status<CR>" "Open Changed File"]
                      :b ["<cmd>Telescope git_branches<CR>" "Checkout Branch"]
                      :c ["<cmd>Telescope git_commits<CR>" "Checkout Commit"]
                      :C ["<cmd>Telescope git_bcommits<CR>" "Checkout Commit (for current file)"]
                      :d {:name "Diff"
                          :d ["<cmd>DiffviewOpen<CR>" "Current Changes"]
                          :m ["<cmd>DiffviewOpen main<CR>" "Against main"]
                          :M ["<cmd>DiffviewOpen master<CR>" "Against master"]
                          :l ["<cmd>DiffviewOpen HEAD~1<CR>" "Against the last commit"]
                          :r ["<cmd>DiffviewRefresh<CR>" "Refresh"]
                          :c ["<cmd>DiffviewClose<CR>" "Close"]}}
                  :l {:name "LSP"
                      :a ["<cmd>lua vim.lsp.buf.code_action()<CR>" "Code Action"]
                      :d ["<cmd>Trouble lsp_document_diagnostics<CR>" "Document Diagnostics"]
                      :D ["<cmd>Telescope lsp_document_diagnostics<CR>" "Search Document Diagnostics"]
                      :w ["<cmd>Trouble lsp_workspace_diagnostics<CR>" "Workspace Diagnostics"]
                      :W ["<cmd>Telescope lsp_workspace_diagnostics<CR>" "Search Workspace Diagnostics"]
                      :t ["<cmd>TroubleToggle<CR>" "Toggle Trouble"]
                      :f ["<cmd>lua vim.lsp.buf.formatting()<CR>" "Format"]
                      :i ["<cmd>LspInfo<CR>" "Info"]
                      :j ["<cmd>lua vim.lsp.diagnostic.goto_next({popop_opts = {border = 'single'}})<CR>"
                          "Next Diagnostic"]
                      :k ["<cmd>lua vim.lsp.diagnostic.goto_prev({popop_opts = {border = 'single'}})<CR>"
                          "Prev Diagnostic"]
                      :q ["<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>" "Quickfix"]
                      :r ["<cmd>lua vim.lsp.buf.rename()<CR>" "Rename"]
                      :s ["<cmd>Telescope lsp_document_symbols<CR>" "Document Symbols"]
                      :S ["<cmd>Telescope lsp_dynamic_workspace_symbols<CR>" "Workspace Symbols"]}
                  :s {:name "Search"
                      :b ["<cmd>Telescope git_branches<cr>" "Git Branches"]
                      :f ["<cmd>Telescope find_files" "Files"]
                      :h ["<cmd>Telescope help_tags<cr>" "Help Tags"]
                      :M ["<cmd>Telescope man_pages<cr>" "Man Pages"]
                      :r ["<cmd>Telescope frecency<cr>" "Recent Files"]
                      :R ["<cmd>Telescope registers<cr>" "Registers"]
                      :t ["<cmd>Telescope live_grep<cr>" "Text"]
                      :c ["<cmd>Telescope commands<cr>" "Commands"]}
                  :r {:name "Replace"
                      :r ["<cmd>lua require'spectre'.open()<CR>" "Replace"]
                      :s ["<cmd>lua require'spectre'.open_visual({select_word = true})<CR>" "Replace Word"]
                      :S ["<cmd>lua require'spectre'.open_visual()<CR>" "Replace Word Again"]
                      :f ["<cmd>lua require'spectre'.open_file_search()<CR>" "Replace in File"]
                      }
                  :T {:name "Treesitter"
                      :i ["<cmd>TSConfigInfo<CR>" "Info"]}}
                 {:mode :n :prefix "<leader>" :buffer nil :silent true :noremap true :nowait true})
    (wk.register {:/ [":CommentToggle<CR>" "Comment"]}
                 {:mode :v :prefix "<leader>" :buffer nil :silent true :noremap true :nowait true})
    (def-augroup :set-local-map
      (def-autocmd :FileType "*" "lua set_local_bindings()"))))
which-key

