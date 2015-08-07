(ns cores.core.one)

(defn first' [[x & _]] x)
(defn last' [[x & xs]] (if xs (last' xs) x))
(defn rest' [[_ & xs]] xs)
(defn butlast' [[x & xs]] (if xs (cons x (butlast' xs)) []))

(defn take'
  [n [x & xs]]
  (if (== n 0) [] (if x (cons x (take' (- n 1) xs)) xs)))

(defn drop'
  [n [x & xs :as lst]]
  (cond (== n 0) lst
        (nil? x) []
        :else (drop' (- n 1) xs)))

(defn take-while'
  [f [x & xs]]
  (cond (nil? x) []
        (f x) (cons x (take-while' f xs))
        :ekse []))

(defn drop-while'
  [f [x & xs :as lst]]
  (cond (nil? x) []
        (f x) (drop-while' f xs)
        :else lst))

(defn ^long sum-sieve
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        primes (boolean-array (+ lim 1) true)
        hlim (+ llim (if (even? llim) 1 2))
        semen (loop [i (int 3) res (int 2)]
                (if (> i llim)
                  res
                  (if (aget primes i)
                    (do (loop [j (int (* i i))]
                          (when (<= j lim)
                            (aset primes j false)
                            (recur (+ j i i))))
                        (recur (+ i 2) (+ res i)))
                    (recur (+ i 2) res))))]
    (loop [i (int hlim) res (int semen)]
      (if (> i lim)
        res
        (if (aget primes i)
          (recur (+ i 2) (+ i res))
          (recur (+ i 2) res))))))

(defn ^longs sieve
  "Too much, and it's slower anyway"
  [^long lim]
  (let [llim (int (Math/sqrt lim))
        hlim (* 6 (inc (quot llim 6)))
        primes (boolean-array (+ lim 1) true)
        semen (loop [i (int 6) res (int 5)]
                (if (>= i llim)
                  res
                  (let [j (- i 1) k (+ i 1)]
                    (if (aget primes j)
                      (if (aget primes k)
                        (do (loop [jj (int (* j j))]
                              (when (<= jj lim)
                                (aset primes jj false)
                                (recur (+ jj j j))))
                            (loop [kk (int (* k k))]
                              (when (<= kk lim)
                                (aset primes kk false)
                                (recur (+ kk k k))))
                            (recur (+ i 6) (+ res j k)))
                        (do (loop [jj (int (* j j))]
                              (when (<= jj lim)
                                (aset primes jj false)
                                (recur (+ jj j j))))
                            (recur (+ i 6) (+ res j))))
                      (if (aget primes k)
                        (do (loop [kk (int (* k k))]
                              (when (<= kk lim)
                                (aset primes kk false)
                                (recur (+ kk k k))))
                            (recur (+ i 6) (+ res k)))
                        (recur (+ i 6) res))))))]
    (loop [i (int hlim) res semen]
      (if (>= i lim)
        res
        (let [j (- i 1) k (+ i 1)]
          (if (aget primes j)
            (if (aget primes k)
              (recur (+ i 6) (+ res j k))
              (recur (+ i 6) (+ res j)))
            (if (aget primes k)
              (recur (+ i 6) (+ res k))
              (recur (+ i 6) res))))))))

(defn permutations
  [[x & xs]]
  (if x
    (let [tmp (permutations xs)]
      (apply concat
             (for [i (range (inc (count xs)))]
               (for [t tmp
                     :let [[a b] (split-at i t)]]
                 (concat a [x] b)))))
    [[]]))
































