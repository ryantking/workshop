(import-macros {:opt-get se?
                :def-command-fn cmd-} "zest.macros")

(local M {})

(local compile-path (.. dirs.config "lua/packer_compiled.lua"))

(var packer nil)

(fn init-packer []
  (cmd "packadd packer.nvim")
  (set packer (require "packer"))
  (packer.init
    {:profile {:enabled true}
     :display {:open_fn (. (require "packer.util") :float)}
     :compile_path compile-path
     :disable_commands true})
  (packer.reset))

(fn load-plugins []
  (-> dirs.modules
      (nvim.fn.globpath "*/plugins.fnl")
      (split "\n")
      (map #(string.match $1 "[%w-/]+/([%w-]+)/plugins.fnl"))
      (map #(.. "modules." $1 ".plugins"))
      (map require)
      (reduce #(merge $1 $2) [])
      (map #(merge [$2] $1))
      (map #(packer.use $1))))

(fn load-packer []
  (init-packer)
  (load-plugins))

(local load-packer-once (once load-packer))

(fn status []
  (load-packer-once)
  (packer.status))

(fn magic-compile [] 
  (load-packer-once)
  (packer.compile "profile=true")
  ; (print (.. "Compiled to " compile-path))
  )

(fn profile []
  (load-packer-once)
  (packer.profile_output))

(fn sync []
  (load-packer-once)
  (packer.sync))

(fn M.auto-compile []
  (-> "%:p"
      (nvim.fn.expand)
      (string.match dirs.modules)
      (when (magic-compile))))

(fn M.load-plugins []
  (if (-> compile-path (vim.fn.filereadable) (= 1))
    (require "_compiled")
    (print "Missing packer file, run PackerCompile to fix"))

  (cmd- :PackerCompile [] (magic-compile))
  (cmd- :PackerStatus [] (status))
  (cmd- :PackerSync [] (sync))
  (cmd- :PackerProfile [] (profile)))

M
