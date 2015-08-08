(ns cores.ml.cluster)

(def sqrt #(Math/sqrt %))

(defn abs [x] (if (pos? x) x (- x)))

(defn sqr [x] (* x x))

(defn distance2
  "Distance between two points in two dimensional cartesian diagram"
  [[x1 y1] [x2 y2]]
  (sqrt (+ (sqr (- y2 y1)) (sqr (- x2 x1)))))

(defn extreme-by
  [f data fby]
  (if (= fby :max)
    (loop [[x & xs] data cur x maxi (f x)]
      (if x
        (let [tmp (f x)]
          (if (> tmp maxi)
            (recur xs x tmp)
            (recur xs cur maxi)))))
    (loop [[x & xs] data cur x mini (f x)]
      (if x
        (let [tmp (f x)]
          (if (< tmp mini)
            (recur xs x tmp)
            (recur xs cur mini)))))))


(defn clustering
  "how-many? categories to be defined, and data should be a collection of 2d points [x y]"
  [how-many? data]
  (let [maxi (extreme-by #(distance2 [0 0] %) data :max)
        mini (extreme-by #(distance2 [0 0] %) data :min)
        length (distance2 maxi mini)
        vlength (let [[xi yi] mini [xm ym] maxi]
                  [(if (> xm xi) (- xm xi) (- xi xm))
                   (if (> ym yi) (- ym yi) (- yi ym))])
        parts (->> (range 1 how-many?)
                   (mapv #(* % (/ length (inc how-many?)))))]))
