;It seems that (require clojure.contrib.math) doesn't work in my clojure.
;So I can only use Java packages.

(import '(java.math BigInteger)
        '(java.lang Math))

(defn gcd [n d]
  (int (. (BigInteger/valueOf n) gcd (BigInteger/valueOf d))))

(defn abs [x] (. Math abs x))
;(defn abs [x] (Math/abs x)) ;It is also okay.

(defn make-rat [n d]
  (let [g (gcd n d)]
    (if (> (* n d) 0)
        (list (abs (/ n g)) (abs (/ d g)))
        (list (- (abs (/ n g))) (abs (/ d g)) ))))

(defn print-rat [x]
  (println (first x) "/" (first (rest x))))

(print-rat (make-rat 8 12))
(print-rat (make-rat -8 12))
(print-rat (make-rat 8 -12))
