(import-macros {:def-keymap ki-
                :def-keymap-fn fki-} "zest.macros")

(require "keymap.globals")

(map
  {"<C-s>" "<cmd>write<CR>"
   "<C-q>" "<cmd>wq<CR>"
   "<C-Q>" "<cmd>q!<CR>"
   "<C-x>" "<cmd>BufferDelete<CR>"
   "Y" "y$"
   "q:" "<NOP>"
   "]b" "<cmd>bp<CR>"
   "[b" "<cmd>bn<CR>"
   "<A-[>" "<cmd>vertical resize +5<CR>"
   "<A-]>" "<cmd>vertical resize -5<CR>"}
  #(ki- $2 [n] $1))

(map
  {"<C-Left>" "<BS>"
   "<C-Right>" "<Del>"
   "<C-Down>" "<Esc>o"
   "<C-Up>" "<Esc>O"
   "<C-s>" "<cmd>write<CR>"
   "<C-q>" "<cmd>wq<CR>"
   "<C-Q>" "<cmd>q!<CR>"}
  #(ki- $2 [i] $1))

(fki- "<C-k>" [n] (ctrl_k))
(ki- "w" [n :remap :silent] "<Plug>WordMotion_w")
(ki- "b" [n :remap :silent] "<Plug>WordMotion_b")
(ki- "gE" [n :remap :silent] "<Plug>WordMotion_gE")
