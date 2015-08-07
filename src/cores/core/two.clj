(ns cores.core.two
  (:require
    [incanter.core :refer :all]
    [incanter.charts :refer :all]
    [incanter.datasets :refer :all]
    [incanter.stats :refer :all]
    [incanter.datasets :refer :all]
    [incanter.bayes :refer :all]))

(def x (range -3 3 0.01))

(defn check2
  []
  (doto (xy-plot x (pdf-normal x)
                 :title "Normal PDF"
                 :y-label "Density"
                 :legend true)
    (add-lines x (pdf-normal x :sd (sqrt 0.2)))
    (add-lines x (pdf-normal x :sd (sqrt 5.0)))
    (add-lines x (pdf-normal x :mean -2 :sd (sqrt 0.5)))
    view))

(defn check
  []
  (doto (xy-plot x (cdf-normal x)
                 :title "Normal CDF"
                 :y-label "Probability"
                 :legend true)
    (add-lines x (cdf-normal x :sd (sqrt 0.2)))
    (add-lines x (cdf-normal x :sd (sqrt 5.0)))
    (add-lines x (cdf-normal x :mean -2 :sd (sqrt 0.5)))
    view))
