(module plugins.tmux {autoload {: tmux : nest}})

(tmux.setup {:navigation {:enable_default_keybindings false}})

(nest.applyKeymaps [["<C-"
                     [["Left>" "<cmd>lua require('tmux').move_left()<CR>"]
                      ["Down>" "<cmd>lua require('tmux').move_bottom()<CR>"]
                      ["Up>" "<cmd>lua require('tmux').move_top()<CR>"]
                      ["Right>" "<cmd>lua require('tmux').move_right()<CR>"]
                      ["S-"
                       [["Left>" "<C-w>H"]
                        ["Down>" "<C-w>J"]
                        ["Up>" "<C-w>K"]
                        ["Right>" "<C-w>L"]]]]]])
