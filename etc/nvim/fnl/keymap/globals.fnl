(local t #(nvim.replace_termcodes $1 true true true))

(global check_back_space
  #(let [col (-> "." (nvim.fn.col) (- 1))]
     (or (= col 0)
         (-> "."
             (nvim.fn.getline)
             (string.sub col col)
             (string.match "%s")))))

(global cmp_tab
  #(if
     (= (nvim.fn.pumvisible) 1) (t "<C-n>")
     (luasnip.expand_or_jumpable) (t "<Plug>luasnip-expand-or-jump")
     (check_back_space) (t "<Tab>")
     ($1)))

(global cmp_stab
  #(if
     (= (nvim.fn.pumvisible) 1) (t "<C-p>")
     (luasnip.expand_or_jumpable -1) (t "<Plug>luasnip-jump-prev")
     ($1)))

(global ctrl_k
  (fn []
    (vim.lsp.buf.signature_help)
    (cmd ":MatchupWhereAmI<CR>")))

(global toggle
  #(->
     $1
     (split " ")
     (map #[$1 (nvim.get_option_info $1)])
     (map (fn [[opt info]] [opt (match info.scope "buf" "bo" "win" "wo" _ "o")]))
     (map (fn [[opt scope]] [opt scope (. vim scope opt)]))
     (map (fn [[opt scope val]] (tset vim scope opt (not val))))))
