(module plugins.galaxyline {autoload {gl galaxyline colors nord.colors condition galaxyline.condition}
                            require-macros [core.macros]})

(def- gls gl.section)

(defn- vi-mode-provider []
  (let [mode-colors {:n colors.nord11_gui :i colors.nord14_gui :v colors.nord10_gui " " colors.nord10_gui
                     :V colors.nord10_gui :c colors.nord9_gui :no colors.nord11_gui :s colors.nord12_gui
                     :S colors.nord12_gui " " colors.nord12_gui :ic colors.nord13_gui :R colors.nord15_gui
                     :Rv colors.nord15_gui :cv colors.nord11_gui :ce colors.nord11_gui :r colors.nord8_gui
                     :rm colors.nord8_gui :r? colors.nord8_gui "!" colors.nord11_gui :t colors.nord11_gui}]
    (vim.api.nvim_command (.. "hi GalaxyViMode guifg=" (. mode-colors (vim.fn.mode)))) "  "))


(tset gl :short_line_list [:NvimTree :packer :dbui])

(tset gls.left 1 {:RainbowRed {:provider (fn [] "▊ ")
                               :highlight [colors.nord10_gui colors.nord2_gui]}})
(tset gls.left 2 {:ViMode {:provider vi-mode-provider
                           :highlight [colors.nord10_gui colors.nord2_gui :bold]}})
(tset gls.left 3 {:FileMode {:provider :FileSize
                             :condition condition.buffer_not_empty
                             :highlight [colors.nord6_gui colors.nord2_gui]}})
(tset gls.left 4 {:FileIcon {:provider :FileIcon
                             :condition condition.buffer_not_empty
                             :highlight [(mod-run :galaxyline.provider_fileinfo :get_file_icon_color)
                                         colors.nord2_gui]}})
(tset gls.left 5 {:FileName {:provider :FileName
                             :condition condition.buffer_not_empty
                             :highlight [colors.nord15_gui colors.nord2_gui]}})
(tset gls.left 6 {:LineInfo {:provider :LineColumn
                             :separator " "
                             :separator_highlight [colors.none colors.nord2_gui]
                             :highlight [colors.nord6_gui colors.nord2_gui]}})
(tset gls.left 7 {:PerCent {:provider :LinePercent
                            :separator " "
                            :separator_highlight [colors.none colors.nord2_gui]
                            :highlight [colors.nord6_gui colors.nord2_gui :bold]}})
(tset gls.left 8 {:DiagnosticError {:provider :DiagnosticError
                                    :icon "  "
                                    :highlight [colors.nord11_gui colors.nord2_gui]}})
(tset gls.left 9 {:DiagnosticWarn {:provider :DiagnosticWarn
                                   :icon "  "
                                   :highlight [colors.nord13_gui colors.nord2_gui]}})
(tset gls.left 10 {:DiagnosticHint {:provider :DiagnosticHint
                                    :icon "  "
                                    :highlight [colors.nord8_gui colors.nord2_gui]}})
(tset gls.mid 1 {:GetLspClient {:provider :GetLspClient
                                :condition (fn [] (and (~= vim.bo.filetype :dashboard) (~= vim.bo.filetype "")))
                                :icon " LSP: "
                                :highlight [colors.nord8_gui colors.nord2_gui :bold]}})
(tset gls.right 1 {:FileEncode {:provider :FileEncode
                                :condition condition.hide_in_width
                                :separator " "
                                :separator_highlight [colors.none colors.nord2_gui]
                                :highlight [colors.nord14_gui colors.nord2_gui :bold]}})
(tset gls.right 2 {:FileFormat {:provider :FileFormat
                                :condition condition.hide_in_width
                                :separator " "
                                :separator_highlight [colors.none colors.nord2_gui]
                                :highlight [colors.nord14_gui colors.nord2_gui :bold]}})
(tset gls.right 3 {:GitIcon {:provider (fn [] "  ")
                             :condition condition.check_git_workspace
                             :separator " "
                             :separator_highlight [colors.none colors.nord2_gui]
                             :highlight [colors.nord15_gui colors.nord2_gui :bold]}})
(tset gls.right 4 {:GitBranch {:provider :GitBranch
                               :condition condition.check_git_workspace
                               :highlight [colors.nord15_gui colors.nord2_gui :bold]}})
(tset gls.right 5 {:DiffAdd {:provider :DiffAdd
                             :condition condition.hide_in_width
                             :icon "  "
                             :separator " "
                             :separator_highlight [colors.none colors.nord2_gui]
                             :highlight [colors.nord14_gui colors.nord2_gui]}})
(tset gls.right 6 {:DiffModified {:provider :DiffModified
                                  :condition condition.hide_in_width
                                  :icon " 柳"
                                  :highlight [colors.nord11_gui colors.nord2_gui]}})
(tset gls.right 7 {:DiffRemove {:provider :DiffRemove
                                :condition condition.hide_in_width
                                :icon "  "
                                :highlight [colors.nord10_gui colors.nord2_gui]}})
(tset gls.right 8 {:RainbowBlue {:provider (fn [] " ▊")
                                 :separator " "
                                 :separator_highlight [colors.none colors.nord2_gui]
                                 :highlight [colors.nord10_gui colors.nord2_gui]}})
(tset gls.short_line_left 1 {:BufferType {:provider :FileTypeName
                                          :separator " "
                                          :separator_highlight [colors.none colors.nord2_gui]
                                          :highlight [colors.nord15_gui colors.nord2_gui :bold]}})
(tset gls.short_line_left 2 {:SFileName {:provider :SFileName
                                         :condition condition.buffer_not_empty
                                         :highlight [colors.nord6_gui colors.nord2_gui :bold]}})
(tset gls.short_line_right 1 {:BufferIcon {:provider :BufferIcon
                                           :highlight [colors.nord6_gui colors.nord2_gui]}})
