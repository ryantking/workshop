(import-macros {:def-augroup gr-
                :def-autocmd-fn fau-} "zest.macros")

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

(fn setup []
    (call "lspconfig" :gopls.setup
      {:capabilities
       (let [c1 (vim.lsp.protocol.make_client_capabilities)
             c2 (call "cmp_nvim_lsp" :update_capabilities c1)]
         c2)
       }
      )
  (print cmp.PreselectMode.None)
  (cmp.setup.buffer
    {:sources
     (->
       ["luasnip" "path"]
       (concat (if (is-ft "go" "lua") ["nvim_lsp"] [{:name "buffer" :keyword_length 5}]))
       (concat (when (is-ft "lua") ["nvim_lua"]))
       (map #(if (= (type $1) "string") {:name $1} $1)))}))

(cmp.setup
  {:preselect cmp.PreselectMode.None
   :snippet {:expand #(luasnip.lsp_expand (. $1 :body))}
   :formatting {:format (lspkind.cmp_format {:with_text true :menu {}})}
   ; :experimental
   ; {:native_menu false
   ;  :ghost_text true}
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

(setup)

(gr- "cmp"
  (fau- "FileType" "*" (setup))
  (fau- "FileType" "TelescopePrompt" (cmp.setup.buffer {:enabled false})))
