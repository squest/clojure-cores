(ns cores.ml.cluster)

(def sqrt #(Math/sqrt %))

(defn abs [x] (if (pos? x) x (- x)))

(defn sqr [x] (* x x))

(defn distance2
  "Distance between two points in two dimensional cartesian diagram"
  [[x1 y1] [x2 y2]]
  (sqrt (+ (sqr (- y2 y1)) (sqr (- x2 x1)))))

(defn extreme-by
  "max-by or min-by"
  [f data fby]
  (if (= fby :max)
    (loop [[x & xs] data cur x maxi (f x)]
      (if x
        (let [tmp (f x)]
          (if (> tmp maxi)
            (recur xs x tmp)
            (recur xs cur maxi)))
        cur))
    (loop [[x & xs] data cur x mini (f x)]
      (if x
        (let [tmp (f x)]
          (if (< tmp mini)
            (recur xs x tmp)
            (recur xs cur mini)))
        cur))))


(defn clustering
  "how-many? categories to be defined, and data should be a collection of 2d points [x y]"
  [how-many? data]
  (let [maxi (extreme-by #(distance2 [0 0] %) data :max)
        mini (extreme-by #(distance2 [0 0] %) data :min)
        vlength (let [[xm ym] maxi [xi yi] mini]
                  [(- xm xi) (- ym yi)])
        parts (->> (range 1 (inc how-many?))
                   (mapv #(* % (/ 1 (inc how-many?))))
                   (map-indexed #(let [[x y] vlength
                                       [xi yi] mini]
                                  {:vec [(+ xi (* %2 x)) (+ yi (* %2 y))]
                                   :cat %1})))
        raw (for [d data :let [tmp (extreme-by #(distance2 d (:vec %)) parts :min)]]
              {:datum d :category (:cat tmp)})
        [cat1 cat2] [(filter #(= 0 (:category %)) raw)
                     (filter #(= 1 (:category %)) raw)]
        [avgx-cat1 avgy-cat1] [(/ (reduce + (map #(first (:datum %)) cat1)) (count cat1))
                               (/ (reduce + (map #(second (:datum %)) cat1)) (count cat1))]
        [avgx-cat2 avgy-cat2] [(/ (reduce + (map #(first (:datum %)) cat2)) (count cat2))
                               (/ (reduce + (map #(second (:datum %)) cat2)) (count cat2))]
        parts2 [{:vec [avgx-cat1 avgy-cat1] :cat 0}
                {:vec [avgx-cat2 avgy-cat2] :cat 1}]]
    (for [d data :let [tmp (extreme-by #(distance2 d (:vec %)) parts2 :min)]]
      {:datum d :category (:cat tmp)})))

(defn clustering-2
  "how-many? categories to be defined, and data should be a collection of 2d points [x y]"
  [how-many? data]
  (let [maxi (extreme-by #(distance2 [0 0] %) data :max)
        mini (extreme-by #(distance2 [0 0] %) data :min)
        vlength (let [[xm ym] maxi [xi yi] mini]
                  [(- xm xi) (- ym yi)])
        parts (->> (range 1 (inc how-many?))
                   (mapv #(* % (/ 1 (inc how-many?))))
                   (map-indexed #(let [[x y] vlength
                                       [xi yi] mini]
                                  {:vec [(+ xi (* %2 x)) (+ yi (* %2 y))]
                                   :cat %1})))]
    (for [d data :let [tmp (extreme-by #(distance2 d (:vec %)) parts :min)]]
      {:datum d :category (:cat tmp)})))

(defn gen-fish
  [n]
  (for [i (repeatedly n #(int (+ 140 (* 40 (rand)))))
        j (repeatedly n #(int (+ 40 (* 40 (rand)))))]
    {:datum  [i j]
     :gender (cond (<= 140 i 175)
                   (cond (<= 40 j 60)
                         (if (<= (rand) 0.8)
                           :female :male)
                         :else (if (<= (rand) 0.8)
                                 :male :female))
                   :else
                   (cond (<= 60 j 80)
                         (if (<= (rand) 0.9)
                           :male :female)
                         :else (if (<= (rand) 0.8)
                                 :female :male)))}))

(defn gen-person
  [n]
  (let [data (-> (fn [x] (let [h (int (+ 140 (* 40 (rand))))
                               w (int (+ (- h 110) (* 20 (rand))))]
                           [h w]))
                 (map (range n)))
        fcat (fn [[h w]]
               {:datum  [h w]
                :gender (if (<= 140 h 160)
                          (if (<= 30 w 60)
                            (if (<= (rand) 0.95) :female :male)
                            (if (<= (rand) 0.2) :female :male))
                          (if (<= 60 w 100)
                            (if (<= (rand) 0.95) :male :female)
                            (if (<= (rand) 0.2) :male :female)))})]
    (mapv fcat data)))

(def people (gen-person 5000))

(defn gender-cluster
  [data]
  (let [cluster (clustering 2 (map :datum data))]
    (map #(merge %1 %2) data cluster)))




















