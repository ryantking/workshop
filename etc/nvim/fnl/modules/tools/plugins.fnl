(local configs (require "modules.tools.config"))

{"nvim-telescope/telescope.nvim"
 {:config configs.telescope
  :cmd "Telescope"
  :requires
  [{1 "nvim-lua/popup.nvim" :opt true}
   {1 "nvim-lua/plenary.nvim" :opt true}
   {1 "nvim-telescope/telescope-fzf-native.nvim" :opt true}
   {1 "nvim-telescope/telescope-frecency.nvim" :opt true}
   {1 "ThePrimeagen/git-worktree.nvim" :opt true}]}

 "lewis6991/gitsigns.nvim"
 {:config configs.gitsigns
  :opt true}

 "TimUntersberger/neogit"
 {:config configs.neogit
  :cmd "Neogit"
  :requires
  {1 "sindrets/diffview.nvim"
   :config configs.diffview
   :cmd ["DiffviewOpen" "DiffviewFileHistory" "DiffviewFileFocus" "DiffviewToggleFiles" "DiffviewRefresh"]}}

 "folke/which-key.nvim"
 {:config #(call "modules.tools.which-key" :config)
  :event "UIEnter"
  ;:keys ["<Space>" "," "g" "z" "c" "d"]
  }

 "henriquehbr/nvim-startup.lua"
 {:config #(call "nvim-startup" :setup)
  :event "VimEnter"}

 "akinsho/nvim-toggleterm.lua"
 {:config configs.nvim-toggleterm
  :keys "<C-t>"}

 "aserowy/tmux.nvim"
 {:config configs.tmux
  :event "VimEnter"}}
