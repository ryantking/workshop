(module options {autoload {nvim aniseed.nvim}
                 require-macros [zest.macros]})

;; Rendering
(opt-set encoding "utf-8")
(opt-set termguicolors)

;; UI
(opt-set number)
(opt-set relativenumber)
(opt-set signcolumn :yes)
(opt-set colorcolumn "81,121")
(opt-set showmatch)
(opt-set matchtime 2)
(opt-set lazyredraw)
(opt-set splitbelow)
(opt-set splitright)
(opt-append shortmess "IcT")

;; Behavior
(opt-set scrolloff 10)
(opt-set sidescroll 1)
(opt-set wrap false)
(opt-set virtualedit :block)
(opt-set undofile)
(opt-set autoread)
(opt-set mouse :a)
(opt-set hidden)
(opt-set wildignore ".git,.git/*,.DS_Store")
(opt-set timeoutlen 500)
(opt-set updatetime 300)
(opt-set history 5000)
(opt-append completeopt [:menuone :noselect])
(opt-set spelloptions :camel)

;; Status Line
(opt-set showmode false)
(opt-set laststatus 2)

;; Search
(opt-set incsearch)
(opt-set inccommand :nosplit)
(opt-set hlsearch)
(opt-set ignorecase)
(opt-set smartcase)

;; Folds
(opt-set foldenable)
(opt-set foldmethod :marker)

;; Spacing
(opt-set tabstop 2)
(opt-set shiftwidth 2)
(opt-set softtabstop 2)
(opt-set expandtab)

;; Invisibles
; (opt-set list)
(opt-set listchars {:trail "␣"})
