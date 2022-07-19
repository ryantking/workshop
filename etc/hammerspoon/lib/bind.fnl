;;; bind.fnl --- Key Binding Library

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

;; This is largely the spacehammer bind lib adapetd to my style.

;;; Code:

(local {: map} hs.fnutils)

(local log (hs.logger.new "bind.fnl" "debug"))

(fn bind-keys
  [items]
  (let [modal (hs.hotkey.modal.new [] nil)]
    (each [_ item (ipairs items)]
      (let [{: key : mods : action} item
            mods (or mods [])]
        (modal:bind mods key nil action)
        (modal:enter)))
    #(when modal (modal:exit) (modal:delete))))

(fn bind-global-keys
  [items]
  (map items
       (fn [item]
         (let [binding (hs.hotkey.bind (or item.mods []) item.key item.action)]
           binding.delete))))

(fn unbind-global-keys
  [bindings]
  (each [_ unbind (ipairs bindings)]
    (unbind)))

{: bind-keys
 : bind-global-keys
 : unbind-global-keys}

;;; bind.fnl ends here
