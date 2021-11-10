(import-macros {:def-autocmd-fn fau-} "zest.macros")

(local cmp (require "cmp"))
(local luasnip (require "luasnip"))
(local lspkind (require "lspkind"))

(fn has-words-before []
  (let [[line col] (nvim.win_get_cursor 0)]
    (and
      (~= col 0)
      (->
        (nvim.buf_get_lines 0 (- line 1) line true)
        (. 1)
        (: :sub col col)
        (: :match "%s")
        (= nil)))))

(lspkind.init)

(cmp.setup
  {:sources
   [{:name "nvim_lua"}
    {:name "nvim_lsp"}
    {:name "path"}
    {:name "luasnip"}
    {:name "buffer" :keyword_length 5}]
   :snippet {:expand #(luasnip.lsp_expand (. $1 :body))}
   :formatting
   {:format (lspkind.cmp_format {:with_text true :menu {}})}
   :experimental
   {:native_menu false
    :ghost_text true}
   :mapping
   {"<C-p>" (cmp.mapping.select_prev_item)
    "<C-n>" (cmp.mapping.select_next_item)
    "<C-d>" (cmp.mapping.scroll_docs -4)
    "<C-f>" (cmp.mapping.scroll_docs 4)
    "<C-Space>" (cmp.mapping.complete)
    "<C-e>" (cmp.mapping.close)
    "<CR>" (cmp.mapping.confirm {:select true})
    "<Tab>"
    (cmp.mapping
      #(if
         (cmp.visible) (cmp.select_next_item)
         (luasnip.expand_or_jumpable) (luasnip.expand_or_jump)
         (has-words-before) (cmp.complete)
         ($1))
      ["i" "s"])
    "<S-Tab>"
    (cmp.mapping
      #(if
         (cmp.visible) (cmp.select_prev_item)
         (luasnip.jumpable -1) (luasnip.jump -1)
         ($1))
      ["i" "s"])}})

(fau- "FileType" "TelescopePrompt" (cmp.setup.buffer {:enabled false}))
