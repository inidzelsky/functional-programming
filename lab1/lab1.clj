;; Task 1
(def l1 `("D" "F" "G" "H" "I" "K"))
(def l2 `(1 2 3 4 5 6 (4 5) 4))
(def l3 `("ER" "RT" "TY" 5 6 6 5))

((fn [l1 l2 l3] 
    (conj `() (first l1) (first l2) (first l3))
  ) l1 l2 l3)

;; Task 2

(def createlist (fn [l1 l2 l3] 
  (list (nth l1 5) (nth l2 3) (nth l3 2)))
)

(createlist l1 l2 l3)

;; Task 3

(def a (hash-set "D" "F" "G" "H" "I" "K"))
(def b (hash-set 1 2 3 4 5 6 (hash-set 4 5) 4))
(def c (hash-set "ER" "RT" "TY" 5 6 6 5))

(defn union [set1 set2] 
  (if (= (count set2) 0) 
    set1
    (union (conj set1 (first set2)) (rest set2)))
)

(defn sub [set1 set2]
  (if (= (count set2) 0)
  set1
  (sub (disj set1 (first set2)) (rest set2))
))

(defn section [set1 set2 set3] 
  (if (= (count set1) 0)
  set3
  (if (contains? set2 (first set1)) 
    (section (rest set1) set2 (conj set3 (first set1))) 
    (section (rest set1) set2 set3)))
)

(sub a (sub (section a b #{}) c))
