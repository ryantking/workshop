(import-macros {:opt-get se?} "zest.macros")

;; Expose nvim to replace vim globally
(global nvim (require "nvim"))

;; Directories
(local home (os.getenv "HOME"))
(global dirs
  {:config (nvim.fn.stdpath "config")
   :cache (.. home "/.cache/nvim/")
   ;:modules (-> "config" (nvim.fn.stdpath) (.. "/fnl/modules"))
   :modules (.. home "/Workshop/etc/nvim/fnl/modules")
   :data (-> "data" (nvim.fn.stdpath) (.. "/site/"))})


;; Expose lume functions globally
(let [lume (require "lume")]
  (global map lume.map)
  (global filter lume.filter)
  (global reduce lume.reduce)
  (global concat lume.concat)
  (global reject lume.reject)
  (global merge lume.merge)
  (global find lume.find)
  (global count lume.count)
  (global split lume.split)
  (global trim lume.trim)
  (global once lume.once)
  (global first lume.first))

;; Call a function in an external module
(global call
  (fn [mod func ...]
    ((-> func (split ".") (reduce #(. $1 $2) (require mod))) ...)))

;; Print and return value, useful for -> chains
(global dprint #(do (print $2) $1))

;; Print the object and return the value, useful for -> chains
(global dump #(do (-> $1 (vim.inspect) (print)) $1))

;; List contains method
(global contains
  (fn [xs x]
    (-> xs
        (map #(= xs x))
        (reduce #(or $1 $2)))))

;; Reverse a sequential table
(global reverse (fn [xs] (reduce xs #(concat [$2] $1) [])))

;; Zip two sequential table into a normal table
(global zip
  (fn [xs ys default]
    (let [[xs ys] (if (<= (lume.count xs) (lume.count ys)) [xs ys] [ys xs])]
      (icollect [ndx x (ipairs xs)] [x (or (. ys ndx) default)]))))

;; Alias to run command
(global cmd #(nvim.command $1))

;; Checks if the file is of a type
(global is-ft
  (fn [ft ...]
    (if
      (= ft (se? ft)) true
      (= (select :# ...) 0) false
      (is-ft ...))))

;; Size of the file in bytes
(global fsize
  #(-> "%:p:f"
     (nvim.fn.expand)
     (nvim.fn.getfsize)
     (or 0)
     (math.max 0)))

;; Class of the file
;;   small:  <  2MB
;;   medium: <  6MB
;;   large:  >= 6MB
(global fclass-small :small)
(global fclass-medium :medium)
(global fclass-large :large)

(global fclass
  #(let [sz (fsize)]
     (if
       (< sz (* 2 1024 1024)) fclass-small
       (< sz (* 6 1024 1024)) fclass-medium
       fclass-large)))

;; Checks if in git project
(global is-git #(-> ".git" (nvim.fn.isdirectory) (= 1)))

;; Guess the master branch.
;; If main or trunk exist, its probably them, otherwire master.
;; Parsing git remote show is too slow.
(global master
  #(if
     (-> "git branch --list main" (vim.fn.system) (length) (> 0)) "main"
     (-> "git branch --list trunk" (vim.fn.system) (length) (> 0)) "trunk"
     "master"))

;; Load a packer plugin if not present
(global packload
  #(let [packer (require "packer")
         plugin (. packer_plugins $1)]
     (when (not (and plugin plugin.loaded))
       (packer.loader $1))))
