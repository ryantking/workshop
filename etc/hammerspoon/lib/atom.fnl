;;; atom.fnl --- Atomic Variables

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

;; This file is ripped straght from spacehammer.

;;; Code:

(fn atom
  [initial]
  {:state initial
   :watchers {}})

(fn copy
  [tbl copies]
  (let [copies (or copies {})]
    (if (~= (type tbl) :table) tbl
        ;; is a table, but already visited
        (. copies tbl)         (. copies tbl)
        ;; else - Is a table, not yet visited
        (let [copy-tbl {}]
          (tset copies tbl copy-tbl)
          (each [k v (pairs tbl)]
            (tset copy-tbl (copy k copies) (copy v copies)))
          (setmetatable copy-tbl (copy (getmetatable tbl) copies))
          copy-tbl))))

(fn deref
  [atom]
  (. atom :state))

(fn notify-watchers
  [atom next-value prev-value]
  (let [watchers (. atom :watchers)]
    (each [_ f (pairs watchers)]
      (f next-value prev-value))))

(fn add-watch
  [atom key f]
  (tset atom :watchers key f))

(fn remove-watch
  [atom key]
  (table.remove (. atom :watchers) key))

(fn swap!
  [atom f ...]
  (let [prev-value (deref atom)
        next-value (f (copy prev-value) (table.unpack [...]))]
    (set atom.state next-value)
    (notify-watchers atom next-value prev-value)
    atom))

(fn reset!
  [atom v]
  (swap! atom (fn [] v)))

{:atom            atom
 :new             atom
 :deref           deref
 :notify-watchers notify-watchers
 :add-watch       add-watch
 :remove-watch    remove-watch
 :reset!          reset!
 :swap!           swap!}

;;; atom.fnl ends here
