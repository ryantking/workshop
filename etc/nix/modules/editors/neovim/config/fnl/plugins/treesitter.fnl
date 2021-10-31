(module plugins.treesitter {autoload {ts nvim-treesitter.configs}})

(ts.setup {:ensure_installed "maintained"
           :highlight {:enable true
                       :use_languagetree true}
           :indent {:enable false}
           :autopairs {:enable true}})
