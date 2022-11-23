;;; core.fnl --- Hammerspoon Config Entrypoint

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop

;; This file is NOT part of Hammerspoon.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file sets up the core of the Hammerspoon configuration and loads extra
;; modules. The majority of this is pulled from spacehammer. I originally wanted
;; to use it, but the modal was not what I wanted for my bindings. It also
;; made yabai's window switching not work well across displays because the modal
;; was acting as a window.

;;; Code:

(local {: contains
        : some
        : split} hs.fnutils)

(local yabai (require :yabai))

(local log (hs.logger.new "\tcore.fnl\t" "debug"))

;; Ensure that the CLI is installed
(hs.ipc.cliInstall)

;;
;;; Utility Functions

(global pprint (fn [x] (print (fennel.view x))))

;;
;;; Config Reloading

(fn source-filename?
  [file]
  "
  Determine if a file is not an emacs backup file which starts with \".#\"
  Takes a file path string
  Returns true if it's a source file and not an emacs backup file.
  "
  (not (string.match file ".#")))

(fn source-extension?
  [file]
  "
  Determine if a file is a .fnl or .lua file
  Takes a file string
  Returns true if file extension ends in .fnl or .lua
  "
  (let [ext (split file "%p")]
    (and
     (or (contains ext "fnl")
         (contains ext "lua"))
     (not (string.match file "-test%..*$")))))

(fn source-updated?
  [file]
  "
  Determine if a file is a valid source file that we can load
  Takes a file string path
  Returns true if file is not an emacs backup and is a .fnl or .lua type.
  "
  (and (source-filename? file)
       (source-extension? file)))

(fn config-reloader
  [files]
  "
  If the list of files contains some hammerspoon or spacehammer source files:
  reload hammerspoon
  Takes a list of files from our config file watcher.
  Performs side effect of reloading hammerspoon.
  Returns nil
  "
  (when (some files source-updated?)
    (hs.console.clearConsole)
    (hs.reload)))

(fn watch-files
  [dir]
  "
  Watches hammerspoon or spacehammer source files. When a file updates we reload
  hammerspoon.
  Takes a directory to watch.
  Returns a function to stop the watcher.
  "
  (let [watcher (hs.pathwatcher.new dir config-reloader)]
    (: watcher :start)
    (fn []
      (: watcher :stop))))

;; Create a global config-files-watcher. Calling it stops the default watcher
(global config-files-watcher (watch-files hs.configdir))

;;
;;; Modules

;; Keybindings
(require :bindings)

;; Stackline
(local stackline (require :stackline))
  (stackline:init)
  (stackline.config:toggle :appearance.showIcons)

;;
;;; Hooks

(global hooks {})

(fn hooks.displayFocused
  []
  (stackline.manager:update {:forceRedraw true}))

(fn hooks.windowDestroyed
  []
  (yabai.on-focused-window
   (fn [win]
     (when (= win.app "Finder")
       (yabai.exec ["-m" "window" "--focus" "recent"])))))

(hs.alert.show "Hammerspoon Config Reloaded")
