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

;; This file defines some functions for interfacing with Yabai.
;; They currently shell out to the yabai CLI with the fast Hammerpsoon
;; task API.

;;; Code:

(local log (hs.logger.new "\tlib.yabai.fnl\t" "debug"))

(local yabai-path (string.gsub (hs.execute "which yabai" true) "%s+" ""))

(fn exec
  [args callback]
  "Pass arguments to the yabai CLI with an optional callback."
  (var stdout "")
  (var stderr "")

  (fn save-output
    [_ new-stdout new-stderr]
    (set stdout (.. stdout new-stdout))
    (set stderr (.. stderr new-stderr))
    true)
  
  (let [task (hs.task.new yabai-path save-output save-output args)]
    (when (= (type callback) :function)
      (task:setCallback
       #(do
          (save-output nil $2 $3)
          (callback $1 stdout stderr))))
    (task:start)))

{: exec}

;;; yabai.fnl ends here
