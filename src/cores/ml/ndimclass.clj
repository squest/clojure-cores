(ns cores.ml.ndimclass)

(def sqrt #(Math/sqrt %))
(def square #(* % %))

(defn ^double distance
  [p1 p2]
  (->> (map #(square (- %2 %1)) p1 p2)
       (reduce +)
       sqrt))

(defn pd-normal
  "Normal probabilistic distribution"
  [{:keys [center deviation height n step]}]
  )
