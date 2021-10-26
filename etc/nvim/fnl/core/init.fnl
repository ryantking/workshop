(import-macros {:let-g g-} "zest.macros")

(fn create-dirs []
  "Create directories if they do not exist"
  (-> ["backup" "session" "swap" "tags" "undo"]
      (map #(.. dirs.cache $1))
      (filter #(-> $1 (nvim.fn.isdirectory) (= 0)))
      (map #(.. "mkdir -p " $1))
      (map #(os.execute))))

(fn disable-builtins []
  "Disable builtin neovim plugins"
  (g- loaded_gzip 1)
  (g- loaded_tar 1)
  (g- loaded_tarPlugin 1)
  (g- loaded_zip 1)
  (g- loaded_zipPlugin 1)
  (g- loaded_getscript 1)
  (g- loaded_getscriptPlugin 1)
  (g- loaded_vimball 1)
  (g- loaded_vimballPlugin 1)
  (g- loaded_matchit 1)
  (g- loaded_matchparen 1)
  (g- loaded_2html_plugin 1)
  (g- loaded_logiPat 1)
  (g- loaded_rrhelper 1)
  (g- loaded_netrw 1)
  (g- loaded_netrwPlugin 1)
  (g- loaded_netrwSettings 1)
  (g- loaded_netrwFileHandlers 1))

(fn setup-leaders []
  "Setup leader keys"
  (g- mapleader " ")
  (g- localmapleader ","))

(require "core.globals")
(create-dirs)
(disable-builtins)
(setup-leaders)
(require "core.options")
(require "core.events")
(call "core.pack" :load-plugins)
(require "keymap")
(require "core.lazy")
