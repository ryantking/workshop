(import-macros {:def-keymap ki-
                :def-keymap-fn fki-} "zest.macros")

(require "keymap.globals")

(ki- [n]
  {"<esc>" ":noh<CR>"
   "Y" "y$"
   "q:" "<NOP>"
   "]b" "<cmd>bp<CR>"
   "[b" "<cmd>bn<CR>"
   "<C-s>" "<cmd>write<CR>"
   "<C-q>" "<cmd>wq<CR>"
   "<C-Q>" "<cmd>q!<CR>"
   "<C-x>" "<cmd>BufferDelete<CR>"
   "<A-[>" "<cmd>vertical resize +5<CR>"
   "<A-]>" "<cmd>vertical resize -5<CR>"
   "<C-p>" "<cmd>Telescope find_files<CR>"})

(ki- [i]
  {"<C-Left>" "<BS>"
   "<C-Right>" "<Del>"
   "<C-Down>" "<Esc>o"
   "<C-Up>" "<Esc>O"
   "<C-s>" "<cmd>write<CR>"
   "<C-q>" "<cmd>wq<CR>"
   "<C-Q>" "<cmd>q!<CR>"})

(ki- [n :remap :silent]
  {"w" "<Plug>WordMotion_w"
   "b" "<Plug>WordMotion_b"
   "gE" "<Plug>WordMotion_gE"})

(fki- "<C-k>" [n] (ctrl_k))
