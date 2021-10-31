(module plugins.autopairs {autoload {npairs nvim-autopairs rule nvim-autopairs.rule
                                     compe nvim-autopairs.completion.compe}})

(compe.setup {:map_cr true
              :map_complete (~= vim.bo.filetype :tex)})

(npairs.setup {:check_ts true
               :ts_config {:java false
                           :javascript ["template_string"]
                           :lua ["string"]}})

(npairs.add_rule (rule "'" "" ["fennel"]))
(npairs.add_rule (rule "`" "" ["fennel"]))
