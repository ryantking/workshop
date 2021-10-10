(module plugins.telescope {autoload {: telescope project project_nvim}})

(telescope.setup {:extensions
                  {:fzf {:case_mode "smart_case"
                         :fuzzy true
                         :override_file_sorter true
                         :override_generic_sorter true}}})

(project.setup {:patterns [".git" "lua" "zshrc"]})

(telescope.load_extension "fzf")
(telescope.load_extension "frecency")
(telescope.load_extension "projects")
