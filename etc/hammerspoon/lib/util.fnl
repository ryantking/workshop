;;; util.fnl --- Utility Functions

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

;; A few utility functions.

;;; Code:

(local {: contains
        : filter
        : reduce} hs.fnutils)

(fn call-when
  [f ...]
  (when (and f (= (type f) :function))
    (f ...)))

(fn concat
  [...]
  (reduce
   [...]
   (fn [cat tbl]
     (each [_ v (ipairs tbl)]
       (table.insert cat v))
     cat)))

(fn has-some?
  [list]
  (and list (< 0 (length list))))

(fn merge
  [...]
  (let [tbls [...]]
    (reduce
     tbls
     (fn merger [merged tbl]
       (each [k v (pairs tbl)]
         (tset merged k v))
       merged))))

{: call-when
 : concat
 : has-some?
 : merge}

;;; util.fnl ends here
