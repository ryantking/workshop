;;; bindings.fnl --- Hammerspoon Key Bindings

;; Copyright (c) 2022  Ryan King <ryantking@protonmail.com>

;; Author: Ryan King <ryantking@rotonmail.com>
;; URL: https://github.com/ryantking/Workshop

;; This file is NOT part of Hammerspoon.

;;; Commentary:

;; Hammerspoon keybindings. Currently these are only for yabai.

;;; Code:

(local yabai (require :yabai))

(local super "cmd-alt")
(local shift-super "cmd-alt-shift")

(macro bind
  [mods key action]
  `(hs.hotkey.bind ,mods (. hs.keycodes.map ,key) ,action))

(bind super "o" #(yabai.dwim-focus-window "next"))
(bind super "n" #(yabai.dwim-focus-window "prev" true))
(bind super "e" #(yabai.exec ["-m" "window" "--ratio" "rel:0.05"]))
(bind super "i" #(yabai.exec ["-m" "window" "--ratio" "rel:-0.05"]))

(bind shift-super "o" #(yabai.dwim-swap-window "next"))
(bind shift-super "n" #(yabai.dwim-swap-window "prev"))
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
(bind super "t" #(yabai.exec ["-m" "window" "--focus" "stack.prev"]))
(bind super "g" #(yabai.exec ["-m" "window" "--focus" "stack.next"]))

;; Space management
(for [n 1 9]
  (bind
   super
   (tostring n)
   #(yabai.exec
     ["-m" "space" "--focus" (tostring n)]
     (fn [code]
       (when (= 0 code)
         (yabai.exec
          ["-m" "query" "--windows" "--space"]
          (fn [code out]
            (let [(ok windows) (pcall hs.json.decode out)]
              (when (and ok (> (length windows) 0))
                (yabai.exec ["-m" "window" "--focus" "first"])))))))))
  
  (bind
   shift-super
   (tostring n)
   #(yabai.on-focused-window 
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

;;; bindings.fnl ends here
