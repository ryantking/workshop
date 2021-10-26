(import-macros {:def-autocmd au-
                :def-autocmd-fn fau-
                :def-augroup gr-} "zest.macros")

(local pack (require "core.pack"))

;(gr- "packer" (fau- "BufWritePost" "*.fnl" (pack.auto-compile)))

(gr- "buffers"
     (au- :BufWritePre "COMMIT_EDITMSG" "setlocal noundofile")
     (au- :BufWritePre "MERGE_MESG" "setlocal noundofile")
     (au- :BufWritePre ".tmp" "setlocal noundofile")
     (au- :BufWritePre ".tmp" "setlocal noundofile")
     (fau- :BufWritePre "*" (vim.lsp.buf.formatting_sync)))

(gr- "windows"
     (au- :VimResized "*" "wincmd =")
     (au- :FocusGained "* checktime"))

(gr- "filetypes"
     (au- :FileType "lspinfo" "nnoremap <silent> <buffer> q :q<CR>")
     (au- :FileType ["text" "latex" "markdown" "gitcommit"] "setlocal wrap"))

(gr- "yank"
  (fau- :TextYankPost "*" (vim.highlight.on_yank {:higroup "IncSearch" :timeout 400})))
