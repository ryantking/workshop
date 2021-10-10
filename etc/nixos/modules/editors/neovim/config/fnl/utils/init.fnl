(module utils {autoload {core aniseed.core nord-util nord.util nord-colors nord.colors}})

(defn reload [name]
  "Requires a lua module and reloads it if already present."
  (tset package.loaded name nil)
  (require name))

(defn mod-run [mod func ...]
  "Runs a function located in an external module."
  ((. (require mod) func) ...))

(defn fcontains? [t f]
  "Returns whether a table contains a value that f returns true for."
  (core.reduce (fn [res kv] (or res (f (unpack kv)))) false (core.kv-pairs t)))

(defn contains? [t v]
  "Returns whether or not table contains a value.
  Sequential values and associated keys are checked."
  (fcontains? t (fn [key value] (or (= key v) (and (= (type key) :number) (= value v))))))

(defn assoc-map [f m]
  "Map m to a new assoc table by calling (f v) on each entry."
  (let [mapped {}]
    (each [k v (pairs m)]
      (tset mapped k (f v)))
    mapped))

