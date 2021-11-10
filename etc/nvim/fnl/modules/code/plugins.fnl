(local configs (require "modules.code.config"))

{;; treesitter base managed by nix due to parser installations

 "nvim-treesitter/nvim-treesitter-textobjects"
 {:config #(call "modules.code.treesitter" :treesitter)
  :opt true}

 "nvim-treesitter/nvim-treesitter-refactor"
 {:config #(call "modules.code.treesitter" :refactor)
  :after "nvim-treesitter-textobjects"}

 "JoosepAlviste/nvim-ts-context-commentstring"
 {:opt true}

 "neovim/nvim-lspconfig"
 {:opt true}

 "jose-elias-alvarez/null-ls.nvim"
 {:config #(call "modules.code.lsp" :null-ls)
  :opt true}

 "hrsh7th/nvim-cmp"
 {:config #(require "modules.code.complete")
  :after ["luasnip" "lspkind-nvim"]
  :requires
  [{1 "hrsh7th/cmp-buffer" :after "nvim-cmp"}
   {1 "hrsh7th/cmp-path" :after "nvim-cmp"}
   {1 "f3fora/cmp-spell" :after "nvim-cmp"}
   {1 "octaltree/cmp-look" :after "nvim-cmp"}
   {1 "hrsh7th/cmp-calc" :after "nvim-cmp"}
   {1 "ray-x/cmp-treesitter" :after "nvim-cmp"}
   {1 "hrsh7th/cmp-nvim-lsp" :after "nvim-cmp"}
   {1 "hrsh7th/cmp-nvim-lua" :after "nvim-cmp"}
   {1 "saadparwaiz1/cmp_luasnip" :after ["nvim-cmp" "luasnip"]}]}

 "onsails/lspkind-nvim"
 {:event "InsertEnter"}

 "L3MON4D3/LuaSnip"
 {:as "luasnip"
  :config configs.luasnip
  :event "InsertEnter"
  :requires {1 "rafamadriz/friendly-snippets" :event "InsertEnter"}}

 "ray-x/navigator.lua"
 {:config #(call "modules.code.lsp" :navigator)
  :requires {1 "ray-x/guihua.lua" :run "(cd lua/fzy && make)"}
  :opt true}

 "ray-x/lsp_signature.nvim"
 {:opt true}

 "folke/trouble.nvim"
 {:after "nvim-web-devicons"
  :opt true}

 "ray-x/go.nvim"
 {:config #(call "modules.code.config" :go)
  :opt true}

 "folke/lua-dev.nvim"
 {:opt true}

 "LnL7/vim-nix"
 {:opt true}

 "bakpakin/fennel.vim"
 {:setup configs.fennel
  :opt true}}
