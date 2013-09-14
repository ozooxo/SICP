(import '(java.lang Math))
(defn expt [x y] (. Math pow x y))
(defn abs [x] (. Math abs x))
(defn log [x] (. Math log x))
(defn sin [x] (. Math sin x))
(defn cos [x] (. Math cos x))

;;;

;Note!! The two functions below works only if every lst is not identical.

(defn list-remove [lst sub-lst]
  (let [sub-set (set sub-lst)]
    (for [x lst :when (not (contains? sub-set x))] x)))
;(println (list-remove '(1 2 3 1 4 5) '(3 1 x))) ;(2 4 5) ;unique terms will be moved completely.

;(defn tmp [lsts]
;  (if (= (count lsts) 1)
;      lsts
;      (apply and (for [lst (rest lsts)] (contains? (set lst) 'x)))))
;(println (tmp '((x y) (y z) (x z))))

;;;

;(defn op? [exp] (and (list? exp) (> (count exp) 2) (symbol? (first exp))))

(defn get-op [exp] (first exp))
(defn get-arg [exp] (second exp))
(defn get-arg-lst [exp] (next exp))

(defn make-function [f x] (list 'function f x))
(defn function? [exp] (and (coll? exp) (= (first exp) 'function)))
(defn get-argument [exp] (nth exp 2)) ;Only one argument (time) is allowed.
;In here, it's a function of time. Or say, it is a generalized coordinate in Lagrangian formulation.

(defn make-deriv [exp var] (list 'deriv exp var))

;;;

(defn gather-num [op-for-num unit-num sequence]
  (let [const (apply op-for-num (for [x sequence :when (number? x)] x))
        others (for [x sequence :when (not (number? x))] x)]
    (cond (and (= const unit-num) (not (empty? others))) others
          (and (= const unit-num) (empty? others)) unit-num 
          :else (cons const others))))
;(println (gather-num + 0 (list 1 'a 2 'b 3 'c 'd))) ;'(6 a b c d)
;(println (gather-num + 0 (list 1 'a 0 'b -1 'c 'd))) ;(a b c d)

(defn merge-same-op [is-op? sequence]
  (concat
   (for [x sequence :when (not (is-op? x))] x)
   (apply concat (for [x sequence :when (is-op? x)] (get-arg-lst x)))))

(defn make-op [op-func op-symb unit-num sequence]
  (let [gathered-seq (gather-num op-func unit-num sequence)]
    (cond (empty? (next gathered-seq)) (first gathered-seq)
          (and (= (first gathered-seq) unit-num) (empty? (nthrest gathered-seq 2))) (second gathered-seq)
          (= (first gathered-seq) unit-num) (cons op-symb (next gathered-seq))
          :else (cons op-symb gathered-seq))))

(defn sum? [x] (and (coll? x) (= (get-op x) '+)))
(defn make-sum [args] (make-op + '+ 0 (merge-same-op sum? args)))
;(println (merge-same-op sum? '(1 2 (+ 3 4) (* 5 6)))) ;(1 2 (* 5 6) 3 4)
;(println (make-sum '(a b (+ 1 c) 3 (* 2 b)))) ;(+ 4 a b (* 2 b) c)

(defn product? [x] (and (coll? x) (= (get-op x) '*)))
(defn make-product [args]
  (let [result (make-op * '* 1 (merge-same-op product? args))]
    (cond (number? result) result
          (symbol? result) result
          (= (second result) 0) 0
          :else result)))
;(println (make-product '(1 a (* 2 f e) b 4 c (+ 4 d)))) ;(* 8 a b c (+ 4 d) f e)
;(println (make-product (list '(+ a b c)))) ;'(+ a b c)

(defn exponentiation? [x] (and (coll? x) (= (first x) '**)))
(defn base [p] (second p))
(defn exponent [p] (nth p 2))
(defn make-exponentiation [x n]
  (cond (= n 0) 1
        (= n 1) x
        (and (number? x) (number? n)) (expt x n)
        (product? x) (make-product (map #(list '** % n) (get-arg-lst x))) ;#(make-exponentiation % n) doesn't work.
        :else (list '** x n)))
;(println (make-exponentiation '(* x y z) 2)) ;(* (** x 2) (** y 2) (** z 2))

(defn abs? [x] (and (coll? x) (= (first x) 'abs)))
(defn log? [x] (and (coll? x) (= (first x) 'log)))
(defn sin? [x] (and (coll? x) (= (first x) 'sin)))
(defn cos? [x] (and (coll? x) (= (first x) 'cos)))

(defn make-abs [x] (if (number? x) (abs x) (list 'abs x)))
(defn make-log [x] (if (number? x) (log x) (list 'log x)))
(defn make-sin [x] (if (number? x) (sin x) (list 'sin x)))
(defn make-cos [x] (if (number? x) (cos x) (list 'cos x)))

;;;

(defn map-derivation [proc op arg-lst]
  (defn map-derivation-recur [prop arg-lst passed-arg-lst result]
    (if (empty? arg-lst)
        result
         (recur prop (next arg-lst) (cons (first arg-lst) passed-arg-lst)
           (cons (concat (reverse passed-arg-lst) (cons (prop (first arg-lst)) (next arg-lst))) result))))
    ;Clojure is really tricky here. You cannot do any operation outside of "recur", so my original approach
    ;(cons (concat ~~~~~) (recur ~~~~~)) does not work.
  (make-sum (map op (map-derivation-recur proc arg-lst '() '()))))

;(println (map-derivation #(+ % 2) #(cons '** %) '(1 2 3))) ;(+ (** 1 2 5) (** 1 4 3) (** 3 2 3))

;;;

(defn deriv [exp var]
  (cond (number? exp) 0
        (= exp var) 1
        (and (symbol? exp) (not (= exp var))) 0
        (function? exp) (if (= (get-argument exp) var)
                            (make-deriv exp var)
                            0);(throw (Exception. "System with more than one argument")))
        (sum? exp) (make-sum (map #(deriv % var) (get-arg-lst exp)))
        (product? exp) (map-derivation #(deriv % var) make-product (get-arg-lst exp))
        (exponentiation? exp) (cond (number? (exponent exp))
                                    (make-product (list (exponent exp)
                                                        (make-exponentiation (base exp) (- (exponent exp) 1))
                                                        (deriv (base exp) var)))
                                    (number? (base exp))
                                    (make-product (list (log (base exp))
                                                        exp
                                                        (deriv (exponent exp) var)))
                                    :else
                                    (make-product (list (make-exponentiation (base exp) (make-sum (list (exponent exp) -1)))
                                                        (make-sum (list (make-product (list (exponent exp)
                                                                                            (deriv (base exp) var)))
                                                                        (make-product (list (base exp)
                                                                                            (deriv (exponent exp) var)
                                                                                            (make-log (base exp)))))))))
        (log? exp) (make-product (list (make-exponentiation (get-arg exp) -1) (deriv (get-arg exp) var)))
        (sin? exp) (make-product (list (make-cos (get-arg exp)) (deriv (get-arg exp) var)))
        (cos? exp) (make-product (list -1 (list (make-sin (get-arg exp)) (deriv (get-arg exp) var))))
        :else (throw (Exception. "unknown expression type -- DERIV" exp))))

;(println (deriv '(+ x 2 x x 3) 'x)) ;3
;(println (deriv '(+ (* x 2) (* x x y 3)) 'x)) ;'(+ 2 (+ (* 3 x y) (* 3 x y)))
;(println (deriv '(** (+ (* 2 x) y) 3) 'x)) ;'(* 6 (** (+ (* 2 x) y) 2))
;(println (deriv '(** 2 (* x y)) 'x)) ;'(* 0.6931471805599453 (** 2 (* x y)) y)
;(println (deriv '(** x x) 'x)) ;'(* (** x (+ -1 x)) (+ x (* x (log x)))) = '(* (** x x) (+ 1 (log x)))
;(println (deriv '(log (** x 3)) 'x)) ;'(* (** (** x 3) -1) (* 3 (** x 2)))
;(println (deriv '(sin (* 3 x)) 'x)) ;'(* 3 (cos (* 3 x)))

;(def xt (make-function 'x 't))
;(println (deriv (list '* xt 3) xt)) ;3
;(println (deriv (list '* xt 3) 't)) ;(* 3 (deriv (function x t) t))
;(println (deriv (list '* xt 3) 's)) ;0
