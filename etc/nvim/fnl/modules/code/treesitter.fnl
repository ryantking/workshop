(local M {})

(fn M.treesitter []
  (let [lines (nvim.fn.line "$")]
    (if (> lines 30000) (ts.setup {:highlight {:enable true}})
      (call "nvim-treesitter.configs" :setup
        {:highlight
         {:enable true
          :additional_vim_regex_highlighting false
          :use_langtree (< lines 7000)}
         :incremental_selection
         {:enable true
          :keymaps
          {:init_selection "gnn"
           :node_incremental "grn"
           :scope_incremental"grc"
           :node_decremental "grm"}}
         :indent {:enable true}
         :textobjects
         {:enable (< lines 7000)
          :lsp_interop
          {:enable (< lines 7000)
           :peek_definition_code
           {:DF "@function.outer"
            :DF "@class.outer"}}
          :keymaps
          {:iL {:go "(function_definition) @function"}
           :af "@function.outer"
           :if "@function.inner"
           :aC "@conditional.outer"
           :iC "@conditional.inner"
           :ac "@class.outer"
           :ic "@class.inner"
           :ae "@block.outer"
           :ie "@block.inner"
           :al "@loop.outer"
           :il "@loop.inner"
           :as "@statement.outer"
           :is "@statement.inner"
           :ad "@comment.outer"
           :am "@call.outer"
           :im "@call.inner"}
          :move
          {:enable (< lines 7000)
           :set_jumps true
           :goto_next_start
           {"]m" "@function.outer"
            "]]" "@class.outer"}
           :goto_next_end
           {"]M" "@function.outer"
            "][" "@class.outer"}
           :goto_previous_start
           {"[m" "@function.outer"
            "[[" "@class.outer"}
           :goto_previous_end
           {"[M" "@function.outer"
            "[]" "@class.outer"}}
          :select
          {:enable (< lines 700)
           :keymaps
           {:af "@function.outer"
            :if "@function.inner"
            :ac "@class.outer"
            :ic "@class.inner"
            :if {:go "(method_declaration) @function"}}}
          :swap
          {:enable (< lines 700)
           :swap_next {"<leader>a" "@parameter.inner"}
           :swap_previous {"<leader>A" "@parmater.inner"}}}}))))

(fn M.refactor []
  (when (< (nvim.fn.line "$") 10000)
    (call "nvim-treesitter.configs" :setup
      {:refactor
       {:highlight_definitions {:enable true}
        :highlight_current_scope {:enable false}
        :smart_rename
        {:enable true
         :keymaps {:smart_rename "<Leader>lr"}}
        :navigation
        {:enable true
         :keymaps
         {:goto_definition "gnd"
          :list_definitions "gnD"
          :list_definitions_toc "gO"}}}
       :matchup {:enable true}
       :autopairs {:enable true}})))

M
