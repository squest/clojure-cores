(ns cores.core-test
  (:require
    [expectations :refer :all]
    [cores.macros :refer :all]
    [cores.core.one :refer :all]))

(testing
  "Square function for macro testing"
  144
  (#(* % %) 12))

(testing-let
  "testing-let macro"
  [botol (range 10)]
  [1 2 3]
  (->> botol (drop 1) (take 3) vec))

(testing
  "Both sieve works the same"
  (dotimes [i 5] (do (println "sieve-2") (time (sieve 2000000))))
  (dotimes [i 5] (do (println "sieve-1") (time (sum-sieve 2000000)))))

