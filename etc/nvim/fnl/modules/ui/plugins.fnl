(local configs (require "modules.ui.config"))

{"kyazdani42/nvim-web-devicons"
 {:requires "lambdalisue/glyph-palette.vim"}

 "famiu/feline.nvim"
 {:event "UIEnter"
  :config configs.feline}

 "romgrk/barbar.nvim"
 {:event "UIEnter"
  :setup configs.barbar
  :requires [{1 "nvim-lua/plenary.nvim" :opt true}]}

 "kyazdani42/nvim-tree.lua"
 {:cmd ["NvimTreeToggle" "NvimTreeOpen" "NvimTreeFindFile"]
  :config configs.nvim-tree}

 "karb94/neoscroll.nvim"
 {:config configs.neoscroll
  :opt true}

 "dstein64/nvim-scrollview"
 {:opt true}

 "lukas-reineke/indent-blankline.nvim"
 {:config configs.indent-blankline
  :opt true}}
