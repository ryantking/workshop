(import-macros {:def-keymap ki-} "zest.macros")

(local M {})

(fn M.go []
  (call "go" :setup
    {:verbose false
     :max_line_len 120
     :filstruct "gopls"
     :log_path (.. (nvim.fn.stdpath "data") "/logs/gonvim.log")
     :lsp_cfg true
     :lsp_codelens false
     :lsp_on_attach #(call "modules.tools.which-key" :add-go-bindings $2)
     }))

(fn M.luasnip []
  (call "luasnip" "config.set_config" {:history true :updateevents "TextChangedI"})
  (ki- "<C-E>" [i] "<Plug>luasnip-next-choice")
  (ki- "<C-E>" [s] "<Plug>luasnip-next-choice"))

(fn M.fennel []
  (g- fennel_fuzzy_indent_patterns
      ["^def" "^let" "^while" "^if" "^fn$" "^var$" "^case$" "^for$" "^each$"
       "^local$" "^global$" "^match$" "^macro" "^lambda$"
       "^map$" "^filter$" "^reject$" "^reduce$" "^call$"
       "^gr-$" "^au-$" "^fau-$" "^ki-$" "^fki-$"]))

M
