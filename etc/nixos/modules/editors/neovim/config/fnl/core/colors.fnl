(module plugins.colors {autoload {colors utils.colors}})

(vim.cmd "syntax on")
(vim.cmd "colorscheme nord")
(vim.cmd (.. "hi IndentBlanklineContextChar guifg=" colors.base0F))

