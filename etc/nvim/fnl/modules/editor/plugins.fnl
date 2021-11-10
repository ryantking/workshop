(local configs (require "modules.editor.config"))

{"tpope/vim-repeat"
 {}

 "tpope/vim-surround"
 {:keys ["c" "d" "S"]}

 "windwp/nvim-autopairs"
 {:config configs.autopairs
  :after "nvim-cmp"}

 "andymass/vim-matchup"
 {:config configs.matchup
  :event ["CursorMoved" "CursorMovedI"]}

 "bfredl/nvim-miniyank"
 {:config configs.miniyank
  :keys ["p" "P" "y" "Y" "<C-v>"]}

 "matze/vim-move"
 {:setup configs.move-pre
  :config configs.move
  :event ["CursorMoved" "CursorMovedI"]}

 "chaoren/vim-wordmotion"
 {:setup configs.wordmotion-pre
  :keys ["<Plug>WordMotion_w" "<Plug>WordMotion_b" "<Plug>WordMotion_gE"]}

 "ggandor/lightspeed.nvim"
 {:setup configs.lightspeed
  :keys ["<Plug>Lightspeed_s" "<Plug>Lightspeed_S"
         "<Plug>Lightspeed_x" "<Plug>Lightspeed_X"
         "<Plug>Lightspeed_f" "<Plug>Lightspeed_F"
         "<Plug>Lightspeed_t" "<Plug>Lightspeed_T"]}

 "yamatsum/nvim-cursorline"
 {:event "UIEnter"}

 "kevinhwang91/nvim-hlslens"
 {:config configs.hlslens
  :keys ["/" "?" "*" "#"]}

 "numToStr/Comment.nvim"
 {:config #(call "Comment" :setup)
  :keys "g"}

 "dhruvasagar/vim-table-mode"
 {:cmd "TableModeToggle"}}
