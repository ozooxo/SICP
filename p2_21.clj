(defn square-list [x]
  (map #(* % %) x))

(println (square-list (list 1 2 3 4))) ;(1 4 9 16)