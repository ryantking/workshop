(import-macros {:let-g g-
                :def-keymap ki-} "zest.macros")

(local M {})

(fn M.treesitter [] (call "modules.code.treesitter" :treesitter))

(fn M.treesitter-refactor [] (call "modules.code.treesitter" :refactor))

(fn M.navigator [] (call "modules.code.lsp" :navigator))

(fn M.null-ls [] (call "modules.code.lsp" :null-ls))

(fn M.cmp [] (require "modules.code.complete"))

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
