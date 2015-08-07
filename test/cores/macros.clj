(ns cores.macros
  (:require [expectations :refer :all]))

(def counter (atom 0))

(defmacro testing
  [msg expected real]
  (do (swap! counter inc)
      (println (str "Testing no " @counter " : " msg))
      `(expect ~expected ~real)))

(defmacro testing-let
  [msg binding expected real]
  (do (swap! counter inc)
      (println (str "Testing no " @counter " : " msg))
      `(expect-let ~binding ~expected ~real)))

