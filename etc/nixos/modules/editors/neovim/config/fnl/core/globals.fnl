(module core.globals {autoload {nvim aniseed.nvim}})

(fn _G.packer [func-name]
  "Call a Packer function and load in the plugin list. Recompiles the plugin modules."
  (let [utils (require :utils)
        cfg-dir (vim.fn.stdpath "config")]
    (utils.mod-run :aniseed.compile :glob "*.fnl" (.. cfg-dir "/fnl/plugins") (.. cfg-dir "/lua/plugins"))
    (utils.reload :plugins.init)
    (utils.mod-run :packer func-name)))

