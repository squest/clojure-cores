(ns cores.notebook.data)

(defn generate-males
  [^long how-many?]
  (for [height (repeatedly how-many? #(+ 150 (* 30 (rand))))
        weight (repeatedly how-many? #(+ 50 (* 40 (rand))))]
    {:gender :male
     :size   [weight height]}))

(defn generate-females
  [^long how-many?]
  (for [height (repeatedly how-many? #(+ 145 (* 35 (rand))))
        weight (repeatedly how-many? #(+ 40 (* 40 (rand))))]
    {:gender :female
     :size   [weight height]}))




