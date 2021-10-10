(module init {autoload {packer packer putil packer.util}
              require-macros [plugins.macros zest.macros]})

(packer.init {:disable_commands true
              :display {:open_fn putil.float
                        :prompt_border :double}})

(def- plenary "nvim-lua/plenary.nvim")
(def- popup "nvim-lua/popup.nvim")
(def- devicons "kyazdani42/nvim-web-devicons")

(def-plugins
  ;; Base
  "wbthomason/packer.nvim" {:event :VimEnter}
  "tsbohc/zest.nvim" {:setup-mod "zest"}
  "Olical/aniseed" {}
  "LionC/nest.nvim" {}
  "folke/which-key.nvim" (use-mod-config :which-key)

  ;; GUI
  "shaunsingh/nord.nvim" {}
  "glepnir/galaxyline.nvim" {:mod "galaxyline" :requires []}
  "romgrk/barbar.nvim" {:requires [devicons]}
  "karb94/neoscroll.nvim" {:setup-mod "neoscroll"}
  "glepnir/dashboard-nvim" {:mod "dashboard"}
  "lukas-reineke/indent-blankline.nvim" {:mod "indent-blankline" :after ["nvim-treesitter"]}
  "dstein64/nvim-scrollview" {}

  ;; Text Editing
  "tpope/vim-repeat" {}
  "tpope/vim-surround" {}
  "nvim-treesitter/nvim-treesitter" {:mod "treesitter" :run ":TSUpdate"}
  "lewis6991/spellsitter.nvim" {:setup-mod "spellsitter" :after ["nvim-treesitter"]}
  "ggandor/lightspeed.nvim" {:event [:BufEnter]}
  "windwp/nvim-autopairs" {:mod "autopairs" :after ["nvim-compe"]}
  "terrortylor/nvim-comment" {:setup-mod "nvim_comment" :event [:BufEnter]}

  ;; File Navigation
  "kyazdani42/nvim-tree.lua" {:event [:BufEnter]}
  "nvim-telescope/telescope-fzf-native.nvim" {:run "make"}
  "nvim-telescope/telescope-frecency.nvim" {:requires ["tami5/sql.nvim"]}
  "ahmedkhalf/project.nvim" {}
  "nvim-telescope/telescope.nvim" {:mod "telescope"}

  ;; Version Control
  "lewis6991/gitsigns.nvim" {:setup-mod "gitsigns" :event [:BufEnter] :requires [plenary]}
  "TimUntersberger/neogit" {:mod "neogit" :requires [plenary "sindrets/diffview.nvim"]}
  "ThePrimeagen/git-worktree.nvim" {:setup-mod "git-worktree"}

  ;; Language Specific
  "ray-x/go.nvim" {:mod "go" :ft ["go" "gomod"]}
  "simrat39/rust-tools.nvim" {:mod "rust" :ft ["rust"] :requires [plenary popup]}
  "folke/lua-dev.nvim" {}

  ;; LSP
  "kabouzeid/nvim-lspinstall" {}
  "jose-elias-alvarez/null-ls.nvim" {}
  "folke/trouble.nvim" {:setup-mod "trouble" :requires [devicons]}
  "ray-x/lsp_signature.nvim" {}
  "ray-x/navigator.lua" {:requires [{1 "ray-x/guihua.lua" :run "cd lua/fzy && make"}]}
  "neovim/nvim-lspconfig" {:mod "lsp"}

  ;; DAP
  "mfussenegger/nvim-dap" {}
  "rcarriga/nvim-dap-ui" {:after ["nvim-dap"]}
  "theHamsta/nvim-dap-virtual-text" {:after ["nvim-dap"]}
  "nvim-telescope/telescope-dap.nvim" {:after ["nvim-dap" "telescope.nvim"]}

  ;; Completion
  "hrsh7th/vim-vsnip" {:event [:InsertEnter]}
  "rafamadriz/friendly-snippets" {:event [:InsertCharPre]}
  "hrsh7th/nvim-compe" {:mod "compe" :event [:InsertEnter]}

  ;; Utilities
  "aserowy/tmux.nvim" {:mod "tmux"}
  "akinsho/nvim-toggleterm.lua" {:mod "term"}
  "ellisonleao/glow.nvim" {:ft [:markdown] :run ":GlowInstall"}
  "windwp/nvim-spectre" {:setup-mod "spectre" :requires [plenary popup]})
