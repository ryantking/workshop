(local M {})

(fn M.feline []
  (packload "nvim-web-devicons")
  (require "modules.ui.feline"))

(fn M.barbar [] (packload "nvim-web-devicons"))

(fn M.nvim-tree []
  (call "nvim-tree" :setup
    {:diagnostics {:enable true}
     :update_focused_file
     {:enable true
      :update_cwd true}}))

(fn M.neoscroll [] (call "neoscroll" :setup))

(fn M.indent-blankline []
  (call "indent_blankline" :setup
     {:buftype_exclude ["terminal" "nofile"]
      :filetype_exclude ["help" "dashboard" "packer"]
      :show_current_context true
      :show_trailing_blankline_indent true
      :context_patterns
      ["class" "function" "method" "^object" "^table" "arguments"]}))

M
