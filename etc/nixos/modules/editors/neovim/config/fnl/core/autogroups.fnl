(module core.autogroups {require-macros [zest.macros]})

(def-augroup :file-type-detect
  (def-autocmd [:BufWinEnter :BufRead :BufNewFile] "*.zsh" "setlocal filetype=sh"))

(def-augroup :file-type-settings
  (def-autocmd :FileType "lspinfo" "nnoremap <silent> <buffer> q :q<CR>")
  (def-autocmd :FileType [:text :latex :markdown :gitcommit] "setlocal wrap spell")
  (def-autocmd-fn :FileType "fennel" [(opt-remove iskeyword ".")
                                      (opt-append lispwords ["string.*" "table.*" "def-augroup"
                                                             "opt-set" "opt-append" "opt-remove"
                                                             "def-autocmd" "def-autocmd-fn"])]))

(def-augroup :flash-yank
  (def-autocmd-fn :TextYankPost "*" [(vim.highlight.on_yank {:higroup2 :Search :timeout 300})]))

(def-augroup :auto-resize
  (def-autocmd :VimResized "*" "wincmd ="))

(def-augroup :autoformat
  (def-autocmd-fn :BufWritePre "*" [(vim.lsp.buf.formatting_sync)]))
