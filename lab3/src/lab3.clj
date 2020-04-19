(ns lab3
    (:require [clojure.string :as str])
    (:gen-class))

(defn read_txt [file] 
 (map 
  (fn [row] 
    (map 
      (fn [s] (str/trim s))) 
      (str/split row #"  ")) 
  (str/split-lines (slurp file))))

(defn read_csv [file]
  (map 
    (fn [row] 
      (str/split row #",")) 
    (str/split-lines (slurp file)))
)

(defn read_tsv [file]
    (map 
    (fn [row] 
      (str/split row #"\t")) 
    (str/split-lines (slurp file)))
)

(defn print_body [ls]
    (dorun (map 
      (fn [row] 
        (dorun (map 
          (fn [s]
            (print (str s "\t"))) 
          row))
          (print "\n")) 
      ls)))


(defn select [file]
  (def typ (subs 
    file 
    (if (> (.lastIndexOf file (int \.)) -1) 
      (.lastIndexOf file (int \.)) 
      0) 
    (count file)))  
  (def res (cond 
    (= typ ".txt") (read_txt file)
    (= typ ".csv") (read_csv file)
    (= typ ".tsv") (read_tsv file)
    :else `()
  ))
  (print_body res)
)
    
(defn test_select [file]
  (try (select file)
  (catch java.io.IOException e (println "Unknow file recognized")))
)

(defn cli [] 
  (println "Enter the title of the file to read")
  (def input (read-line))
  (cond 
    (= input "quit") (println "End of the programm")
    :else ((test_select (subs input (+ (.indexOf input (int \")) 1) (.lastIndexOf input (int \")))) (cli))))

(defn -main []
  (cli)
)

