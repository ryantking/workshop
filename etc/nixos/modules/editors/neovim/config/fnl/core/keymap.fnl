(module core.keymap {autoload {nvim aniseed.nvim core aniseed.core}
                     require-macros [core.macros]})

(global mode-adapters
  {:normal :n
   :insert :i
   :term :t
   :visual :v
   :visual-block :x})

(defn load-mode [mode keymap]
  (each [key val (pairs keymap)]
    (let [(binding opts)
          (if (core.table? val) 
            (values (. val 1) (. val 2))
            (values val {:silent true :noremap (not (= mode :t))}))]
      (nvim.set_keymap mode key binding opts))))

(defn load [keymap]
  (each [mode-name keymap (pairs keymap)]
    (let [mode (. mode-adapters mode-name)]
      (if mode
        (load-mode mode keymap)
        (print (string.format "invalid mode: %s" mode))))))

(g :mapleader " ")
(g :maplocalleader ",")
(nvim.command "nnoremap q: <NOP>")

; (load
;   {:normal
;    {"<C-p>" ":Telescope find_files <CR>"}})

