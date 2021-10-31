{:def-plugins
 (fn [...]
   `(let [{:assoc assoc# :count count# :merge merge#} (require :aniseed.core)
          {:startup startup#} (require :packer)
          pkgs# (assoc# {} ,...)]
      (startup# (fn [use#]
                  (each [name# opts# (pairs pkgs#)]
                    (use# (merge# [name#] opts#
                                  (when opts#.mod {:config (.. "require'plugins." opts#.mod "'")})
                                  (when opts#.setup-mod {:config (.. "require'" opts#.setup-mod "'.setup {}")})
                                  )))))))

 :use-ext-config
 (fn [name]
   `(let [configs# (require :plugins.configs)
          cfg# (. configs# ,name)]
      (if cfg# cfg#
          (print (.. "plugin config not found: " ,name)))))

 :use-mod-config
 (fn [mod]
   `(let [mod# (.. "plugins." ,mod)]
      (tset package.loaded mod# nil)
      (let [(ok?# mod-or-err#) (pcall require mod#)]
        (if ok?# mod-or-err#
            (print (.. "plugin modules not found: " mod#))))))

 :run-mod-func
 (fn [mod func opts]
   (let [a (require :aniseed.core)]
     `(let [mod# (require ,mod)]
        (var func# ,(if (a.table? func) `mod# `(. mod# ,func)))
        ,(when (a.table? func)
           `(each [ndx# key# (ipairs ,func)]
              (set func# (. func# key#))))
        (func# ,opts))))

 :run-mod-setup
 (fn [mod opts]
   `(let [mod# (require ,mod)
          func# (. mod# :setup)]
      (func# ,opts)))

 :use-mod-setup
 (fn [mod opts ...]
   `(let [{:assoc assoc#} (require :aniseed.core)]
      (assoc# {:config
               (fn []
                 (let [mod# (require ,mod)
                       func# (. mod# :setup)]
                   (func# ,opts))) }
              ,...)))

 :load-settings
 (fn [name]
   `(let [settings# (. (require :settings) :plugins ,name)]
      (if settings# settings#
          (print (.. "settings not found for plugin: " ,name)))))

 :use-colorcheme
 (fn [name]
   `{:config (fn [] (vim.cmd (.. "colorscheme " ,name)))})

 :load-telescope-ext
 (fn [name]
   `(let [{:load_extension load#} (require :telescope)] (load# ,name)))

 :use-telescope-ext
 (fn [name ...]
   `(let [a# (require :aniseed.core)]
      (a#.assoc {:config
                 (fn []
                   (let [{:load_extension load#} (require :telescope)] (load# ,name)))
                 :after ["telescope.nvim"]} ,...)))}

; (fn M.def-plugins [...]
;   `(let [a# (require :aniseed.core)
;          packer# (require :packer)
;          pkgs# (a#.assoc {} ,...)]
;      (packer#.startup
;        (fn [use#]
;          (each [name# opts# (pairs pkgs#)]
;            (use# (a#.merge [name#] opts#)))))))


