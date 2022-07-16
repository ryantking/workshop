;;; macros.fnl --- Common Macros

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

;; Some cool macros taken from spacehammer.

;;; Code:

(fn when-let
  [[var-name value] body1 ...]
  (assert body1 "expected body")
  `(let [,var-name ,value]
     (when ,var-name
       ,body1 ,...)))

(fn if-let
  [[var-name value] body1 ...]
  (assert body1 "expected body")
  `(let [,var-name ,value]
     (if ,var-name
       ,body1
       ,...)))

(fn time
  [body1 ...]
  (assert body1 "expected body")
  `(let [start# (os.clock)
         results# (do ,body1 ,...)
         end# (os.clock)
         diff# (- end# start#)]
     (print "Executed in" diff# " seconds.")
     results#))

{:when-let when-let
 :if-let if-let
 :time time}

;;; macros.fnl ends here
