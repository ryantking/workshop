(import-macros {:opt-set se- :opt-append se+} :zest.macros)

;; Rendering
(se- encoding "utf-8")
(se- termguicolors)
(se- fileformats "unix,mac,dos")

;; UI
(se- number)
(se- relativenumber)
(se- signcolumn :yes)
(se- colorcolumn "81,121")
(se- showmatch)
(se- matchtime 2)
(se- lazyredraw)
(se- splitbelow)
(se- splitright)
(se+ shortmess "IcT")

;; Behavior
(se- hidden)
(se- mouse "nv")
(se- scrolloff 10)
(se- sidescroll 1)
(se- wrap false)
(se- virtualedit :block)
(se- autoread)
(se- timeoutlen 500)
(se- updatetime 300)
(se- history 5000)
(se- completeopt ["menu" "menuone" "noselect"])
(se- spelloptions :camel)
(se- wildignore ".git,.git/*,.DS_Store")
(se- wildignorecase)
(se- textwidth 120)

;; Aux Files
(se- undofile)
(se- backup false)
(se- writebackup false)
(se- swapfile false)
(se- directory (.. dirs.cache "swag/"))
(se- undodir (.. dirs.cache "undo/"))
(se- backupdir (.. dirs.cache "backup/"))
(se- viewdir (.. dirs.cache "view/"))
(se- spellfile (.. dirs.cache "spell/en.utf-8.add"))

;; Status Line
(se- showmode false)
(se- laststatus 2)

;; Search
(se- incsearch)
(se- inccommand :nosplit)
(se- hlsearch)
(se- ignorecase)
(se- smartcase)

;; Folds
(se- foldenable)
(se- foldmethod :marker)

;; Spacing
(se- tabstop 2)
(se- shiftwidth 2)
(se- softtabstop 2)
(se- expandtab)
(se- smarttab)

;; Invisibles
(se- list)
(se- listchars "tab:»·,nbsp:+,trail:·,extends:→,precedes:←")
