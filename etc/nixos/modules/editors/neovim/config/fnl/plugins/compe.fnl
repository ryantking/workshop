(module plugins.compe {autoload {: compe : nest} require-macros [zest.macros]})

(let-g vsnip_snippet_dir (.. (os.getenv :HOME) "/.config/nvim/snippets"))

(compe.setup {:enabled true
              :autocomplete true
              :debug false
              :min_length 1
              :preselect "enable"
              :throttle_time 80
              :source_timeout 200
              :incomplete_delay 400
              :max_abbr_width 100
              :max_kind_width 100
              :max_menu_width 100
              :documentation {:border "single"
                              :winhighlight "NormalFloat:CompeDocumentation,FloatBorder:CompeDocumentationBorder"
                              :max_width 120
                              :min_width 60
                              :max_height (math.floor (* vim.o.lines 0.3))
                              :min_height 1}
              :source {:path {:kind "  "}
                       :buffer {:kind "   "}
                       :calc {:kind "   "}
                       :vsnip {:kind "   "}
                       :nvim_lsp {:kind "  "}
                       :nvim_lua false
                       :spell {:kind "   "}
                       :tags false
                       :vim_dadbod_completion false
                       :snippets_nvim false
                       :ultisnips false
                       :treesitter false}})

(defn- t [s]
  (vim.api.nvim_replace_termcodes s true true true))

(defn- check-back-space []
  (let [col (- (vim.fn.col ".") 1)]
    (or (= col 0) (string.match (string.sub (vim.fn.getline ".") col col) "%s"))))

(defn- tab_complete []
  (if (= (vim.fn.pumvisible) 1) (t "<C-n>")
      (if (= (vim.fn.call "vsnip#jumpable" [1]) 1) (t "<Plug>(vsnip-jump-next)")
          (if (check-back-space) (t "<Tab>") (t "<Tab>")))))

(defn- s_tab_complete []
  (if (= (vim.fn.pumvisible) 1) (t "<C-p>")
      (if (= (vim.fn.call "vsnip#jumpable" [-1]) 1) (t "<Plug>(vsnip-jump-prev)")
          (t "<S-Tab>"))))

(nest.applyKeymaps [{:mode :i :options {:expr true}
                     1 [["<Tab>" (vlua-format "%s()" tab_complete)]
                        ["<S-Tab>" (vlua-format "%s()" s_tab_complete)]]}
                    {:mode :s :options {:expr true}
                     1 [["<Tab>" (vlua-format "%s()" tab_complete)]
                        ["<S-Tab>" (vlua-format "%s()" s_tab_complete)]]}])
