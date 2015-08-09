(ns cores.ml.ndimclass)

(def sqrt #(Math/sqrt %))
(def square #(* % %))

(defn ^double distance
  [p1 p2]
  (->> (map #(square (- %2 %1)) p1 p2)
       (reduce +)))

(defn normal-pdf
  "Takes the mean and standard deviation, producing function to produce sample data"
  [mean sd])


