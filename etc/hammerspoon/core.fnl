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

(require-macros :macros)

(local {: contains
        : map
        : reduce
        : some
        : split} hs.fnutils)

(local yabai (require :lib.yabai))

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
(global config-files-watcher (watch-files configdir))

;;
;;; Window Borders

(local border-canvas (hs.canvas.new {:x 0 :y 0 :w 100 :h 100}))

(fn border-color
  [win space]
  (if
   (or win.is-floating (= space.type "float")) {:hex "#A3BE8C" :alpha 1}
   win.has-fullscreen-zoom {:hex "#B48EAD" :alpha 1}
   true {:hex "#88C0D0" :alpha 1}))

(fn draw-border
  [win]
  (yabai.exec
   ["-m" "query" "--spaces" "--space"]
   (fn [code stdout]
     (when (= code 0)
       (let [space (hs.json.decode stdout)]
         (when (and win border-canvas)
           (border-canvas:topLeft {:x win.frame.x :y (- win.frame.y 2)})
           (border-canvas:size {:w (+ win.frame.w 4) :h (+ win.frame.h 4)})
           (border-canvas:replaceElements
            {:type "rectangle"
             :action "stroke"
             :strokeColor (border-color win space)
             :strokeWidth 4
             :roundedRectRadii {:xRadius 10 :yRadius 10}
             :padding 2})
           (border-canvas:show)))))))

(fn clear-border
  [win]
  (border-canvas:hide))

;;
;;; Bindings

(local super "cmd-alt")
(local shift-super "cmd-alt-shift")

(fn on-window
  [])

(fn on-focused-window
  [callback]
  (yabai.exec
   ["-m" "query" "--windows" "--window"]
   #(do (when (and (= $1 0) $2 (> (length $2) 0))
          (callback (hs.json.decode $2))))))

(macro bind
  [mods key action]
  `(hs.hotkey.bind ,mods (. hs.keycodes.map ,key) ,action))

;; Left and right style navigation loosely inspired by paper

(fn dwim-focus-window
  [dir last-window]
  (yabai.exec
   ["-m" "window" "--focus" dir]
   (fn [code]
     (when (> code 0)
       (yabai.exec
        ["-m" "display" "--focus" dir]
        (fn [code]
          (when (= code 0)
            (yabai.exec ["-m" "window" "--focus" (if last-window "last" "first")]))))))))

(fn dwim-swap-window
  [dir]
  (yabai.exec
   ["-m" "window" "--swap" dir]
   (fn [code]
     (when (> code 0)
       (on-focused-window
        (fn [window]
          (yabai.exec
          ["-m" "window" "--display" dir]
          (fn [code]
            (when (= code 0)
              (yabai.exec ["-m" "window" "--focus" window.id]))))))))))


(bind super "o" #(dwim-focus-window "next"))
(bind super "n" #(dwim-focus-window "prev" true))
(bind super "e" #(yabai.exec ["-m" "window" "--ratio" "rel:0.05"]))
(bind super "i" #(yabai.exec ["-m" "window" "--ratio" "rel:-0.05"]))

(bind shift-super "o" #(dwim-swap-window "next"))
(bind shift-super "n" #(dwim-swap-window "prev"))
(bind shift-super "e" #(yabai.exec ["-m" "window" "--ratio" "rel:-0.1"]))
(bind shift-super "i" #(yabai.exec ["-m" "window" "--ratio" "rel:0.1"]))

;; Floating movement
(bind super "/" #(yabai.exec ["-m" "window" "--toggle" "float"]))

(bind super "left" #(yabai.exec ["-m" "window" "--move" "rel:-64:0"]))
(bind super "up" #(yabai.exec ["-m" "window" "--move" "rel:0:-64"]))
(bind super "down" #(yabai.exec ["-m" "window" "--move" "rel:0:64"]))
(bind super "right" #(yabai.exec ["-m" "window" "--move" "rel:64:0"]))

;; TODO: These should maybe make a little more sense on tiled windows (or be disapled)
(bind shift-super "left" #(yabai.exec ["-m" "window" "--resize" "right:-64:0"]))
(bind shift-super "up" #(yabai.exec ["-m" "window" "--resize" "bottom:0:-64"]))
(bind shift-super "down" #(yabai.exec ["-m" "window" "--resize" "bottom:0:64"]))
(bind shift-super "right" #(yabai.exec ["-m" "window" "--resize" "right:64:0"]))

;; Stacking
(bind super "t" #(yabai.exec ["-m" "window" "--focus" "stack.next"]))
(bind super "g" #(yabai.exec ["-m" "window" "--focus" "stack.prev"]))

;; Space management
(for [n 1 9]
  (bind
   super
   (tostring n)
   #(yabai.exec
     ["-m" "space" "--focus" (tostring n)]
     (fn [code]
       (when (= 0 code)
         (yabai.exec ["-m" "window" "--focus" "first"])))))
  
  (bind
   shift-super
   (tostring n)
   #(on-focused-window 
     (fn [window]
       (yabai.exec
        ["-m" "window" "--space" (tostring n)]
        (fn [code]
          (when (= code 0)
            (yabai.exec
             ["-m" "space" "--focus" (tostring n)]
             (fn [code]
               (when (= code 0)
                 (yabai.exec ["-m" "window" "--focus" (tostring window.id)])))))))))))

;; Layout management
(bind super "b" #(yabai.exec ["-m" "space" "mouse" "--layout" "bsp"]))
(bind super "s" #(yabai.exec ["-m" "space" "mouse" "--layout" "stack"]))
(bind super "f" #(yabai.exec ["-m" "space" "mouse" "--layout" "float"]))

;; Rotate windows
(bind super "." #(yabai.exec ["-m" "space" "--rotate" "270"]))
(bind super "," #(yabai.exec ["-m" "space" "--rotate" "90"]))

;; Fullscreen
(bind super "m" #(yabai.exec ["-m" "window" "--toggle" "zoom-fullscreen"]))

;;
;;; Hooks

(global hooks {})

(var current-focus nil)

(fn hooks.windowFocused
  [id]
  (on-focused-window
   (fn [win]
     (if win
         (when (or (= current-focus nil) (not= current-focus.id win.id))
           (set current-focus win)
           (draw-border win))
         (do
           (set current-focus nil)
           (clear-border))))))

(fn hooks.windowResized
  [id]
  (when (and current-focus (= current-focus.id id))
    (on-focused-window
     (fn [win]
       (draw-border win)))))

(fn hooks.windowMoved
  [id]
  (when (and current-focus (= current-focus.id id))
    (on-focused-window
     (fn [win]
       (draw-border win)))))

(fn hooks.applicationActivated
  [id]
  (hooks.windowFocused id))

(fn hooks.windowDestroyed
  []
  (yabai.exec ["-m" "window" "--focus" "first"]))

(hooks.windowFocused nil)

(hs.alert.show "Hammerspoon Config Reloaded")
