;;; yabai.fnl --- Yabai Bindings

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

;; This file sets up functions for interacting with yabai from Hammerspoon then
;; creates a series of hotkeys based on the StumpWM binding scheme. I choose
;; StumpWM's binding scheme because it was designed with Emacs in mind. Instead
;; of using a few dozen bindings using modifier chords, it puts everything in a
;; modal binding behind `C-t` so it doesn't conflict with any system or Emacs
;; bindings. Also if I setup StumpWM on a Linux box, I'll have the same
;; exact bindings, which will make switching back and forth easy.

;;; Code:

(local {: map
        : split} (require :hs.fnutils))

(local log (hs.logger.new "\tyabai.fnl\t" "debug"))

(local yabai-path (string.gsub (hs.execute "which yabai" true) "%s+" ""))

(fn yabai
  [args on-success on-error]
  (var yabai_out "")
  (var yabai_err "")
  
  (fn capture-output
    [task stdout stderr]
    (when stdout (set yabai_out (.. yabai_out stdout)))
    (when stderr (set yabai_err (.. yabai_err stderr )))
    true)

  (fn callback
    [stdout stderr]
    (when (> (length stdout) 0) (log.df "out: %s" stdout))
    (if (= (length stderr) 0)
        (when on-success (on-success))
        (do
          (log.df "err: %s" stderr)
          (when on-error (on-error)))))

  (log.df "yabai %s" args)
  (let [task (hs.task.new yabai-path #(print) capture-output (split args "%s"))]
    (task:setCallback #(callback yabai_out yabai_err))
    (task:start)))

(fn focus-display
  [sel]
  (yabai (.. "-m display --focus " sel)))

(fn swap-display
  [sel]
  (yabai (.. "-m window --display " sel)
         (partial focus-display sel)))

(fn focus-window
  [dir]
  (yabai (.. "-m window --focus " dir)
         nil
         (partial focus-display dir)))

(fn swap-window
  [dir]
  (yabai (.. "-m window --swap " dir) nil
         (partial swap-display dir)))

(fn warp-window
  [dir]
  (yabai (.. "-m window --warp " dir) nil
         (partial swap-display dir)))

(fn focus-space
  [sel]
  (yabai (.. "-m space --focus " sel)
         (partial focus-window "first")))

(fn swap-space
  [sel]
  (yabai (.. "-m window --space " sel)
         (partial focus-space sel)))

{: focus-window
 : swap-window
 : warp-window
 : focus-space
 : swap-space}
