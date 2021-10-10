(module plugins.dashboard {require-macros [zest.macros]})

(let-g dashboard_custom_header
       [" ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗"
        " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║"
        " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║"
        " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║"
        " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║"
        " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝"])
(let-g dashboard_custom_shortcut
       {:last_session "SPC s l"
        :find_history "SPC f h"
        :find_file "SPC f  "
        :new_file "SPC c n"
        :change_colorscheme "SPC t c"
        :find_word "SPC s t"
        :book_marks "SPC f b"})
