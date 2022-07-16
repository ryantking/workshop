;;; modal.fnl --- More sophisticated modal

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

;; The builtin Hammerspoon modal only handles a single list of bindings.
;; This modal is a wrapper around the builtin modal that uses a state
;; machine to allow nested bindings for more complicated binding schemes.
;; Most of this is ripped from spacehammer, but I ripped out the UI and
;; config portion.

;;; Code:

(require-macros :macros)

(local atom (require :lib.atom))
(local machine (require :lib.statemachine))
(local {: bind-keys} (require :lib.bind))
(local {: call-when : has-some? : merge} (require :lib.util))
(local {: contains
        : concat
        : filter
        : find
        : map} hs.fnutils)

;;
;;; Utilities

(local log (hs.logger.new "modal.fnl" "debug"))

(fn timeout
  [f]
  (let [task (hs.timer.doAfter 2 f)]
    #(when task (: task :stop) nil)))
  
(fn by-key
  [target]
  (fn [item]
    (and (= item.key target)
         (has-some? item.items))))

;;
;;; Actions

(fn activate-modal
  [fsm menu-key]
  (fsm.send :activate menu-key))

(fn enter-modal
  [fsm menu-key]
  (fsm.send :enter menu-key))

(fn deactivate-modal
  [fsm]
  (fsm.send :deactivate))

;;
;;; Keybindings

(fn create-action-trigger
  [fsm {: action}]
  (fn []
    (fsm.deactivate)
    (action)))

(fn create-menu-trigger
  [fsm {: key}]
  (fn []
    (fsm.enter key)))

(fn select-trigger
  [fsm item]
  (if item.action
      (create-action-trigger fsm item)
      item.items
      (create-menu-trigger fsm item)
      (fn []
        (log.w "no trigger cuold be found for item: " (hs.inspect item)))))

(fn bind-item
  [fsm item]
  {:mods (or item.mods [])
   :key item.key
   :action (select-trigger fsm item)})

(fn bind-menu-keys
  [fsm items]
  (-> items
      (filter #(or $1.action $1.items))
      (map (partial bind-item fsm))
      (concat [{:key :ESCAPE
                :action deactivate-modal}
               {:mods [:ctrl]
                :key "g"
                :action deactivate-modal}])
      (bind-keys)))

;;
;;; State Transition Functions

(fn ->menu
  [fsm state action menu-key]
  {:state
   {:current-state :active
    :context
    (merge state.context
           {:menu (if menu-key
                      (find state.context.menu.items (by-key menu-key))
                      state.context.bindings)
            :cancel-timeout (timeout fsm.deactivate)})}
   :effect :open-modal})

(fn active->idle
  [fsm state action extra]
  {:state {:current-state :idle
           :context (merge state.context {:menu nil})}
   :effect :close-modal})

;;
;;; Lifecycle Methos

(fn enter-menu
  [menu]
  (when (and menu menu.enter) (menu:enter)))

(fn exit-menu
  [menu]
  (when (and menu menu.exit) (menu:exit)))

(fn open-modal
  [fsm state]
  (enter-menu state.context.menu)
  (let [unbind-keys (bind-menu-keys fsm state.context.menu.items)
        cancel-timeout state.context.cancel-timeout]
    (fn []
      (unbind-keys)
      (call-when cancel-timeout fsm)
      (exit-menu state.context.menu))))

;;
;;; Finite State Machine

(fn enable-logging
  [fsm]
  (atom.add-watch
   fsm.state :log-state
   (fn log-state
     [state]
     (log.df "state: %s" state.current-state))))

(fn create-modal
  [bindings]
  (let [initial-context {:bindings bindings
                         :menu  nil}
        template {:state {:current-state :idle
                          :context initial-context}
                  :states        {:idle {:activate ->menu}
                                  :active {:deactivate active->idle
                                           :enter ->menu}}
                  :log "modal"}
        fsm (machine.new template)
        modal-effect (machine.effect-handler {:open-modal (partial open-modal fsm)})]
    (tset fsm :enable-logging (partial enable-logging fsm))
    (tset fsm :activate (partial activate-modal fsm))
    (tset fsm :deactivate (partial deactivate-modal fsm))
    (tset fsm :enter (partial enter-modal fsm))
    (fsm.subscribe modal-effect)
    (fsm.enable-logging)
    fsm))

{:new create-modal}

;;; modal.fnl ends here
