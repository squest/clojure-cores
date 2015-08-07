(ns cores.core.three
  (:require
    [incanter.core :refer :all]
    [incanter.charts :refer :all]
    [incanter.datasets :refer :all]
    [incanter.stats :refer :all]
    [incanter.datasets :refer :all]
    [incanter.bayes :refer :all]))


(defn make-sea-bass []
  ;; sea bass are mostly long and light in color
  #{:sea-bass
    (if (< (rand) 0.2) :fat :thin)
    (if (< (rand) 0.7) :long :short)
    (if (< (rand) 0.8) :light :dark)})

(defn make-salmon []
  ;; salmon are mostly fat and dark
  #{:salmon
    (if (< (rand) 0.8) :fat :thin)
    (if (< (rand) 0.5) :long :short)
    (if (< (rand) 0.3) :light :dark)})

(defn make-sample-fish []
  (if (< (rand) 0.3) (make-sea-bass) (make-salmon)))

(def fish-training-data
  (for [i (range 10000)] (make-sample-fish)))

(defn probability
  "Calculates the probability of a specific category
   given some attributes, depending on the training data."
  [attribute & {:keys [category prior-positive prior-negative data]
                :or   {category nil
                       data     fish-training-data}}]
  (let [by-category (if category
                      (filter category data)
                      data)
        positive (count (filter attribute by-category))
        negative (- (count by-category) positive)
        total (+ positive negative)]
    (/ positive total)))

(defn evidence-of-salmon [& attrs]
  (let [attr-probs (map #(probability % :category :salmon) attrs)
        class-and-attr-prob (conj attr-probs
                                  (probability :salmon))]
    (float (apply * class-and-attr-prob))))

(defn evidence-of-seabass [& attrs]
  (let [attr-probs (map #(probability % :category :sea-bass) attrs)
        class-and-attr-prob (conj attr-probs
                                  (probability :sea-bass))]
    (float (apply * class-and-attr-prob))))

(defn probability-of-which?
  [fish & attrs]
  (let [sea-bass? (apply evidence-of-seabass attrs)
        salmon? (apply evidence-of-salmon attrs)]
    ({:salmon (/ salmon? (+ sea-bass? salmon?))} fish
      (/ sea-bass? (+ sea-bass? salmon?)))))
