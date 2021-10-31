(module plugins.term {autoload {: toggleterm}})

(toggleterm.setup {:open_mapping "<C-t>"
                   :shade_terminals false
                   :shell (.. "IN_NEOVIM=true " vim.o.shell)
                   :size 20})
