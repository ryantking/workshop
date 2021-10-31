(module plugins.go {autoload {: go} require-macros [zest.macros]})

(go.setup {:verbose true
           :log_path (.. (vim.fn.expand "$HOME") "/.config/nvim/logs/gonvim.log")
           :lsp_codelens false
           :gopls_cmd [(.. (vim.fn.stdpath "data") "/lspinstall/go/gopls")]})

(def-augroup "goimports"
  (def-autocmd :BufWritePre "*.go" ":lua require'go.format'.goimport()"))
