;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns cores.notebook.one
  (:require [gorilla-plot.core :refer :all]
            [cores.ml.cluster :refer :all]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn render 
  [f some-data]
  (let [data (gender-cluster f some-data)
      fmale #(and (= (:gender %) :male)
                             (= (:category %) 1))
      ffemale #(and (= (:gender %) :female)
                             (= (:category %) 0))
      [a b c] [(filter fmale data)
               (filter ffemale data)
               (filter #(not (or (fmale %) (ffemale %))) data)]]
  (println "Male " (count a) " Female " (count b) " Miscategorised " (count c))
      (compose (list-plot (map :datum a) :colour "blue" :symbol-size 10
                          :plot-range [[130 190] [30 100]])
               (list-plot (map :datum b) :colour "red" :symbol-size 10)
               (list-plot (map :datum c) :colour "yellow" :symbol-size 10))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cores.notebook.one/render</span>","value":"#'cores.notebook.one/render"}
;; <=

;; @@
(defn gen-person
  [n]
  (let [data (-> (fn [x] (let [h (int (+ 140 (* 40 (rand))))
                               w (int (+ (- h 110) (* 30 (rand))))]
                           [h w]))
                 (map (range n)))
        fcat (fn [[h w]]
               {:datum  [h w]
                :gender (if (<= 140 h 160)
                          (if (<= 30 w 65)
                            (if (<= (rand) 0.97) :female :male)
                            (if (<= (rand) 0.05) :female :male))
                          (if (<= 60 w 100)
                            (if (<= (rand) 0.95) :male :female)
                            (if (<= (rand) 0.07) :male :female)))})]
    (mapv fcat data)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cores.notebook.one/gen-person</span>","value":"#'cores.notebook.one/gen-person"}
;; <=

;; @@
(def people (gen-person 1000))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cores.notebook.one/people</span>","value":"#'cores.notebook.one/people"}
;; <=

;; @@
(defn gender-cluster
  [f data]
  (let [cluster (f 2 (map :datum data))]
    (map #(merge %1 %2) data cluster)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cores.notebook.one/gender-cluster</span>","value":"#'cores.notebook.one/gender-cluster"}
;; <=

;; @@
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cores.notebook.one/clustering-2</span>","value":"#'cores.notebook.one/clustering-2"}
;; <=

;; @@
(do (render clustering people) nil)
;; @@
;; ->
;;; Male  474  Female  456  Miscategorised  70
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defn clustering-3
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
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;cores.notebook.one/clustering-3</span>","value":"#'cores.notebook.one/clustering-3"}
;; <=

;; @@
(do (render clustering-2 people) nil)
;; @@
;; ->
;;; Male  467  Female  455  Miscategorised  78
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(do (render clustering-3 people) nil)
;; @@
;; ->
;;; Male  474  Female  456  Miscategorised  70
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
