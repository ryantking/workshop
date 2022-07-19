;;; statemachine.fnl --- A simple state machine library

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

;; A simple state machine library that I adapted from spacehammer as
;; with most of my library functions. I just replaced the spacehammer
;; functional lib with hammerspoon's builtin one.

;;; Code:

(require-macros :macros)

(local atom (require :lib.atom))
(local {: call-when
        : merge} (require :lib.util))

(fn update-state
  [fsm state]
  (atom.swap! fsm.state (fn [_ state] state) state))

(fn get-transition-function
  [fsm current-state action]
  (. fsm.states current-state action))

(fn get-state
  [fsm]
  (atom.deref fsm.state))

(fn send
  [fsm action extra]
  (let [state (get-state fsm)
        {: current-state : context} state]
    (if-let [tx-fn (get-transition-function fsm current-state action)]
            (let [
                  transition (tx-fn fsm state action extra)
                  new-state (if transition transition.state state)
                  effect (if transition transition.effect nil)]

              (update-state fsm new-state)
                                        ; Call all subscribers
              (each [_ sub (pairs (atom.deref fsm.subscribers))]
                (sub {:prev-state state :next-state new-state : action : effect : extra}))
              true)
            (do
              (if fsm.log
                  (fsm.log.df "Action :%s does not have a transition function in state :%s"
                              action current-state))
              false))))

(fn subscribe
  [fsm sub]
  (let [sub-key (tostring sub)]
    (atom.swap! fsm.subscribers (fn [subs sub]
                                  (merge {sub-key sub} subs)) sub)
    ; Return the unsub func
    (fn []
      (atom.swap! fsm.subscribers (fn [subs key] (tset subs key nil) subs) sub-key))))

(fn effect-handler
  [effect-map]
  ;; Create a one-time atom used to store the cleanup function
  (let [cleanup-ref (atom.new nil)]
    ;; Return a subscriber function
    (fn [{: prev-state : next-state : action : effect : extra}]
      ;; Whenever a transition occurs, call the cleanup function, if set
      (call-when (atom.deref cleanup-ref))
      ;; Get a new cleanup function or nil and update cleanup-ref atom
      (atom.reset! cleanup-ref
                   (call-when (. effect-map effect) next-state extra)))))

(fn create-machine
  [template]
  (let [fsm  {:state (atom.new {:current-state template.state.current-state :context template.state.context})
              :states template.states
              :subscribers (atom.new {})
              :log (if template.log (hs.logger.new template.log "info"))}]
    ; Add methods
    (tset fsm :get-state (partial get-state fsm))
    (tset fsm :send (partial send fsm))
    (tset fsm :subscribe (partial subscribe fsm))
    fsm))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

{: effect-handler
 : send
 : subscribe
 :new create-machine}

;;; statemachine.fnl ends here
