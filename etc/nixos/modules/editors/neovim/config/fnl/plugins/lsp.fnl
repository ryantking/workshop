(module plugins.lsp {autoload {str aniseed.string : lspconfig : navigator : null-ls
                               nls-helpers null-ls.helpers wk which-key luadev lua-dev}})

(def- completion-item-kind
  ["   (Text)"
   "   (Method)"
   "   (Function)"
   "   (Constructor)"
   " ﴲ  (Field)"
   "[] (Variable)"
   "   (Class)"
   " ﰮ  (Interface)"
   "   (Module)"
   " 襁 (Property)"
   "   (Unit)"
   "   (Value)"
   " 練 (Enum)"
   "   (Keyword)"
   "   (Snippet)"
   "   (Color)"
   "   (File)"
   "   (Reference)"
   "   (Folder)"
   "   (EnumMember)"
   " ﲀ  (Constant)"
   " ﳤ  (Struct)"
   "   (Event)"
   "   (Operator)"
   "   (TypeParameter)"])

(tset vim.lsp.protocol "CompletionItemKind" completion-item-kind)

(defn- on-attach [client bufnr]
  (wk.register {:g {:r "References"
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

(navigator.setup {:debug true
                  :border "single"
                  :lspinstall true
                  :keymaps [{:key "gd" :func "definition()"}]
                  :on_attach on-attach
                  :icons {:code_action_icon " "
                          :code_lens_action_icon " "
                          :diagnostic_head ""
                          :diagnostic_err ""
                          :diagnostic_warn ""
                          :diagnostic_info ""
                          :diagnostic_hint ""
                          :diagnostic_head_severity_1 ""
                          :diagnostic_head_severity_2 ""
                          :diagnostic_head_severity_3 ""
                          :diagnostic_head_description " "
                          :diagnostic_virtual_text ""
                          :diagnostic_file " "
                          :value_changed ""
                          :value_definition ""
                          :match_kinds {:var " "
                                        :method "ƒ "
                                        :function " "
                                        :parameter "  "
                                        :associated ""
                                        :namespace ""
                                        :type " "
                                        :field "ﴲ"}
                          :treesitter_defult ""}
                  :lsp {:format_on_save true
                        :disable_format_cap ["lua" "go"]
                        :code_lens true
                        :diagnostic_scroll_bar_sign ["▃" "█"]
                        :sumneko_lua (luadev.setup {:library {:vimruntime true
                                                              :types true
                                                              :plugins true}
                                                    :lspconfig {:cmd [(.. (vim.fn.stdpath :data) "/lspinstall/lua/./sumneko-lua-language-server")]
                                                                :Lua {:diagnostics {:globals ["vim"]}}}})}})

(null-ls.config {})

(lspconfig.null-ls.setup {})

(def- linters ["shellcheck" "hadolint" "markdownlint"])
(each [_ linter (ipairs linters)]
  (null-ls.register (. null-ls.builtins.diagnostics linter)))

(def- formatters ["prettierd" "shellharden" "shfmt" "stylua"])
(each [_ formatter (ipairs formatters)]
  (null-ls.register (. null-ls.builtins.formatting formatter)))

(defn- parse-fennel-output [params done]
  (if (not params.output) (done)
    (let [lines (str.split params.output "\n")
          message (str.trim (. lines 2))
          row (string.match (. lines 1) ":(%d+)")
          col (length (. params.content (tonumber row)))]
      (done [{:severity 1 : message : row : col}]))))

; (null-ls.register
;   {:name "fennel"
;    :method null-ls.methods.DIAGNOSTICS
;    :filetypes ["fennel"]
;    :generator (nls-helpers.generator_factory
;                 {:command "fennel"
;                  :args ["--globals" "jit,unpack" "--raw-errors" "-"]
;                  :output "raw"
;                  :to_stdin true
;                  :from_stderr true
;                  :on_output
;                   (nls-helpers.diagnostics.from_patterns
;                     [{:pattern "[%w-/]+:(%d+): Parse error: (.*)"
;                       :groups [:row :message]}])})})
; 
