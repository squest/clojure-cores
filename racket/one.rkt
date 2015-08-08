#lang racket

(require math)
(require plot)

(define fheight (normal-dist 156 10))
(define fweight (normal-dist 50 7))
(define mheight (normal-dist 165 10))
(define mweight (normal-dist 63 9))

(define satu (plot3d (surface3d (λ (x y) (+ (* 2 x x) (* -5 y y)))
                                -10 10 -10 10)))


(define (distance p1 p2)
  (let* ((x1 (first p1))
         (y1 (second p1))
         (x2 (first p2))
         (y2 (second p2)))
    (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1))))))

(define (extreme-by f lst fkey)
  (define (loop xs cur ext)
    (if (empty? xs)
        cur
        (let* ((x (first xs))
               (tmp (f x))
               (fk (if (eq? "max" fkey) > <)))
          (if (fk tmp ext)
              (loop (rest xs) x tmp)
              (loop (rest xs) cur ext)))))
  (loop lst (first lst) (f (first lst))))

(define (clustering xs)
  (let* ((mini (extreme-by (λ (x) (distance '(0 0) x)) xs "min"))
         (xmin (first mini))
         (ymin (second mini))
         (maxi (extreme-by (λ (x) (distance '(0 0) x)) xs "max"))
         (xmax (first maxi))
         (ymax (second maxi))
         (xlen (- xmax xmin))
         (ylen (- ymax ymin))
         (cat1 (list (+ (* 1/3 xlen) xmin)
                     (+ (* 1/3 ylen) ymin)))
         (cat2 (list (+ (* 2/3 xlen) xmin)
                     (+ (* 2/3 ylen) ymin)))
         (fcat (λ (p) (extreme-by (λ (x) (distance x p)) `(,cat1 ,cat2) "min"))))
    (map (λ (x) (if (eq? (fcat x) cat1)
                    (reverse (cons "F" (reverse x)))
                    (reverse (cons "M" (reverse x)))))
         xs)))

(define (gen-people n)
  (append (for/list ((w (map round (sample fweight n)))
                     (h (map round (sample fheight n))))
            (list w h "female"))
          (for/list ((w (map round (sample mweight n)))
                     (h (map round (sample mheight n))))
            (list w h "male"))))

(define people (clustering (gen-people 500)))

(define (chart data)
  (let* ((fmale (λ (x) (and (eq? (third x) "male") (eq? (fourth x) "M"))))
         (ffemale (λ (x) (and (eq? (third x) "female") (eq? (fourth x) "F"))))
         (males (map (λ (x) (list (first x)(second x)))
                     (filter fmale data)))
         (females (map (λ (x) (list (first x)(second x)))
                       (filter ffemale data)))
         (missed (map (λ (x) (list (first x)(second x)))
                      (filter (λ (o) (not (or (fmale o) (ffemale o)))) data))))
    (map println `(,(length males) ,(length females) ,(length missed)))
    (plot (list (points males #:color "blue")
                (points females #:color "red")
                (points missed #:color "yellow")))))
         
         
         
         
         
         
         
         
         
         
         
         
         
         
         