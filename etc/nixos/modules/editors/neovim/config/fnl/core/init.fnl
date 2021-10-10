(module core {autoload {nvim aniseed.nvim}})

;; Packer commands that will compile/reload the config before calling the
;; underlying packer functions
(nvim.command "silent! command PackerInstall call v:lua.packer('install')")
(nvim.command "silent! command PackerCompile call v:lua.packer('compile')")
(nvim.command "silent! command PackerStatus call v:lua.packer('status')")
(nvim.command "silent! command PackerSync call v:lua.packer('sync')")
(nvim.command "silent! command PackerUpdate call v:lua.packer('update')")

(require :core.globals)
(require :core.colors)
(require :core.options)
(require :core.autogroups)
(require :core.keymap)

