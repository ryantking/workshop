(import-macros {:opt-get se?
                :def-keymap ki-} "zest.macros")

(local M {})

(fn M.telescope []
  (let [telescope (require "telescope")]
    (telescope.setup
      {:extensions
       {:fzf {:case_mode "smart_case"
              :fuzzy true
              :override_file_sorter true
              :override_generic_sorter true}}})

    ; (call "project_nvim" :setup
      ; {:detection_methods ["pattern"]
       ; :patterns [".git" "lua"]})

    (packload "telescope-fzf-native.nvim")
    (telescope.load_extension "fzf")
    (packload "telescope-frecency.nvim")
    (telescope.load_extension "frecency")
    ;(telescope.load_extension "projects")
    (when (contains packer_plugins :git_worktree)
      (telescope.load_extension "git_worktree"))))

(fn M.gitsigns [] (call "gitsigns" :setup))

(fn M.diffview [] (call "diffview" :setup {}))

(fn M.neogit []
  (call "neogit" :setup
    {:signs
     {:section ["▶" "▼"]
      :item ["▶" "▼"]
      :hunk ["" ""]}
     :integrations {:diffview true}}))

(fn M.which-key [] (require "modules.tools.which-key"))

(fn M.toggleterm []
  (call "toggleterm" :setup
    {:open_mapping "<C-t>"
     :shade_terminals false
     :shell (se? shell)
     :size 20}))

(fn M.tmux []
  (call "tmux" :setup {:navigation {:enable_default_keybindings false}})

  (ki- "<C-Left>" [n] "<cmd>lua require'tmux'.move_left()<CR>")
  (ki- "<C-Down>" [n] "<cmd>lua require'tmux'.move_bottom()<CR>")
  (ki- "<C-Up>" [n] "<cmd>lua require'tmux'.move_top()<CR>")
  (ki- "<C-Right>" [n] "<cmd>lua require'tmux'.move_right()<CR>")

  (ki- "<C-S-Left>" [n] "<C-w>H")
  (ki- "<C-S-Down>" [n] "<C-w>J")
  (ki- "<C-S-Up>" [n] "<C-w>K")
  (ki- "<C-S-Right>" [n] "<C-w>L"))

M
