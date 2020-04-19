;; Task 1
(defun lastelem (lst) 
  (if (null (cdr lst)) 
    (car lst) 
    (lastelem (cdr lst))))

(defun conseq (num lst)
  (cond
    ((= num 0) lst)
    ((= (list-length lst) 0) (conseq (- num 1) (list 1)))
    ((= (list-length lst) 1) (conseq (- num 1) (append lst (list (* (car lst) -2)))))
    ((> (list-length lst) 1) (conseq (- num 1) (append lst (list (* (lastelem lst) -2)))))
    ))

(format t "~%Task #1~%Consequence : ~a~%" (conseq 2 `()))

;; Task 2
(defun swap (lst newlst i j ind)
  (cond 
    ((>= ind (list-length lst)) newlst)
    ((= ind i) (swap lst (append newlst (list (nth j lst))) i j (+ ind 1)))
    ((= ind j) (swap lst (append newlst (list (nth i lst))) i j (+ ind 1)))
    ((= 1 1) (swap lst (append newlst (list (nth ind lst))) i j (+ ind 1)))))

(defun inner (lst j D type)
  (if (>= (- j D) 0)
    (cond
      ((STRING= type "num") 
        (if (< (nth j lst) (nth (- j D) lst)) 
          (inner (swap lst `() j (- j D) 0) (- j D) D type) 
          (inner lst (- j D) D type)))
      ((STRING= type "alpha") 
        (if (STRING< (nth j lst) (nth (- j D) lst))
          (inner (swap lst `() j (- j D) 0) (- j D) D type) 
          (inner lst (- j D) D type)))
      ((STRING= type "len") 
        (if (< (length (nth j lst)) (length (nth (- j D) lst))) 
          (inner (swap lst `() j (- j D) 0) (- j D) D type) 
          (inner lst (- j D) D type))))
    lst))

(defun middle (lst len i D type) 
  (if (= i len) 
    lst 
    (middle (inner lst i D type) len (+ i 1) D type)))

(defun outer (lst D type) 
  (if (= D 1)
    lst
    (outer (middle lst (list-length lst) (floor (/ D 2.0)) (floor (/ D 2.0)) type) (floor (/ D 2.0)) type)))


(defun shellsort (lst type)
  (outer lst (floor (/ (list-length lst) 2.0)) type))

(format t "~%Task #2~%Shellsort on numeric cryteria : ~a~%" (shellsort `(128 3740 90 5 5 43 2 1) "num"))

;;Task 3
 (format t "~%Task #3~%Shellsort on alphabet cryteria : ~a ~%~%" (shellsort `("Excepteur" "sint" "occaecat" "cupidatat" "non" "proident", "sunt" "in" "culpa" "qui" "officia" "deserunt"
 "mollit" "anim" "id" "est" "laborum.") "alpha"))

(format t "Shellsort on length cryteria : ~a ~%~%" (shellsort `("Excepteur" "sint" "occaecat" "cupidatat" "non" "proident", "sunt" "in" "culpa" "qui" "officia" "deserunt"
"mollit" "anim" "id" "est" "laborum.") "len"))

  


