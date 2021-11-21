(local M {})

(local lua-dev
  (let [(ok ld) (pcall require "lua-dev")]
    (if ok (l.setup {:lspconfig {:settings {:Lua {:completion {:showWord "disable"}}}}}) {})))

(fn M.navigator []
  (call "navigator" :setup
    {:debug true
     :width 0.7
     :lsp_installer false
     :border "single"
     ; :ts_fold true
     ; :keymaps [{:key "gd" :func "definition()"}]
     :on_attach #(call "keymap.leader" :register-lsp $2)
     :icons
     {
      ; :code_lens_action_icon " "
      ; :diagnostic_head ""
      :diagnostic_err "⬥"
      :diagnostic_warn "⬥"
      :diagnostic_info "⬥"
      :diagnostic_hint "⬥"
      ; :diagnostic_head_severity_1 ""
      ; :diagnostic_head_severity_2 ""
      ; :diagnostic_head_severity_3 ""
      ; :diagnostic_head_description " "
      :diagnostic_virtual_text "•"
      ; :diagnostic_file " "
      ; :value_changed ""
      ; :value_definition ""
      :match_kinds
      {:var " "
       :method "ƒ "
       :function " "
       :parameter "  "
       :associated ""
       :namespace ""
       :type " "
       :field "ﴲ"}}
     :lsp
     {:format_on_save true
      :disable_format_cap ["lua" "go"]
      :code_lens true
      :diagnostic_scroll_bar_sign ["▃" "█"]
      :sumneko_lua lua-dev
      :gopls {:on_attach (fn [client _] (tset client.resolved_capabilities :document_formatting false))}}}))

(fn M.null-ls []
  ; (let [{: config : generator : builtins : methods} (require "null-ls")
  ;       linters ["codespell" "shellcheck" "markdownlint"]
  ;       formatters ["fish_indent" "nixfmt" "prettierd" "shfmt" "shellharden" "stylua" "trim_newlines" "trim_whitespace"]
  ;       fennel-linter
  ;       {:name "fennel"
  ;        :method methods.DIAGNOSTICS
  ;        :filetypes ["fennel"]
  ;        :generator
  ;        (generator
  ;          {:command "fennel"
  ;           :args ["--compile" "--raw-errors" "-"]
  ;           :format "line"
  ;           :to_stdin true
  ;           :from_stderr true
  ;           :check_exit_code #(<= $1 1)
  ;           :on_output
  ;           (call "null-ls.helpers" "diagnostics.from_pattern"
  ;             "-:(%d+): Parse error: (.+)" ["row" "message"])})}]
  ;
  ;   (config
  ;     {:sources
  ;      (concat
  ;        [fennel-linter]
  ;        (map linters #(. builtins.diagnostics $1))
  ;        (map formatters #(. builtins.formatting $1)))})
  ;
  ;   (call "lspconfig" "null-ls.setup" {})
  ;   )
  )

M
