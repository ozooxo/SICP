(import '(java.lang Math))
(defn expt [x y] (. Math pow x y))

(defn cons' [a b]
  (long (* (expt 2 a) (expt 3N b))))

(defn mod-times [x a]
  (defn mod-times' [x a n]
  (if (= (rem x a) 0)
      (recur (/ x a) a (+ n 1))
      n))
  (mod-times' x a 0))

(defn car' [n] (mod-times n 2))

(defn cdr' [n] (mod-times n 3))

(println (car' (cons' 14 20))) ;14
(println (cdr' (cons' 14 20))) ;20