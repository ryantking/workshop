(module plugins.indent-blankline {autoload {ibl indent_blankline colors nord.colors : settings}})

(ibl.setup {:buftype_exclude ["terminal" "nofile"]
            :filetype_exclude ["dashboard"]
            :show_current_context true
            :show_trailing_blankline_indent true
            :context_patterns ["class" "function" "method" "^object" "^table" "arguments"]})
