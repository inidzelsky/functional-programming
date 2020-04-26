(ns lab5
    (:require [clojure.string :as str])
    (:gen-class)
)

;; Swaps 2 elements of the vector 
(defn swap 
  ([table i1 i2] (if (<= i1 i2) (swap table i1 i2 true) (swap table i2 i1 true)))
  ([table i1 i2 bool] 
    (vec (concat (conj (subvec table 0 i1) (nth table i2)) (conj (subvec table (inc i1) i2) (nth table i1)) (subvec table (inc i2) (count table))))))

(defn comp_mech [row1 row2 col]
      (try 
        (> (compare (Integer/parseInt (nth row1 col)) (Integer/parseInt (nth row2 col))) 0)
      (catch Exception e (> (compare (nth row1 col) (nth row2 col)) 0))))

(defn quick_sort [table col_ind]
  (defn partiate [table i j]
    (defn start [table i]
      (if (and (< i (count table)) (comp_mech (nth table 0) (nth table i) col_ind))
        (recur table (inc i))
        i))
        
    (defn finish [table j]
      (if (and (> j 0) (comp_mech (nth table j) (nth table 0) col_ind))
        (recur table (dec j))
        j))
        
    (def ni (start table i))
    (def nj (finish table j))

    (if (< ni nj) 
      (recur (swap table ni nj) (inc ni) (dec nj))
      (hash-map :table table :pointer ni)))


  (if (<= (count table) 1)
    table
    (let [res (partiate table 1 (dec (count table)))]
      (vec 
        (concat
          (quick_sort (subvec (res :table) 1 (res :pointer)) col_ind)
          [(first (res :table))]
          (quick_sort (subvec (res :table) (res :pointer) (count (res :table))) col_ind))))))
  
;; Checks whether according elements of 2 vectors are equal 
(defn rows_equal? [row1 row2] 
  (if (and (empty? row1) (empty? row2))
    true
    (if (= (first row1) (first row2))
      (recur (rest row1) (rest row2))
      false)))

;; Unites 2 tables
(defn unite [table1 table2] 
  (if (empty? table1)
    (vec table2)
    (if (> (.indexOf (map (fn [row] (rows_equal? (first table1) row)) table2) true) -1)
      (unite (rest table1) table2)
      (unite (rest table1) (conj table2 (first table1))))))

;; Intersects 2 tables
(defn intersect [table1 table2] 
  (if (empty? table1)
    `()
    (vec (remove nil? (conj 
      (seq (intersect (rest table1) table2))
      (if (> (.indexOf (map (fn [row] (rows_equal? (first table1) row)) table2) true) -1)
        (first table1)
        nil))))))

;; Finds in every column the length of the longest element
(defn find_max_length [table]
  (defn compare_vectors [vec1 vec2]
    (if (empty? vec1)
      (seq vec2)
      (if (empty? vec2) 
        (seq vec1) 
        (conj (compare_vectors (rest vec1) (rest vec2)) (if (> (first vec1) (first vec2)) (first vec1) (first vec2))))))

  (defn run_rows_max [table result] 
    (if (empty? table)
      result
      (recur (rest table) (compare_vectors result (map (fn [element] (count element)) (first table))))))

  (vec (run_rows_max table [])))

;; Creates a string of len length of symb
(defn add_symbols [len symb] 
  (if (> len 0)
    (str symb (add_symbols (dec len) symb))
    ""))

;; Defines whether files vector includes table
(defn read_file [table_name files_vec]
  (if (empty? files_vec) 
    false 
    (if (= (first files_vec) table_name) 
      true
      (recur table_name (rest files_vec)))))

;; Reads .txt files
(defn read_txt [file_name]
  (defn split_handler [row]
    (if (> (count (str/split row #"\t")) 1)
      (str/split row #"\t")
      (str/split row #"  ")))

  (map 
    (fn [row] (split_handler row))
    (str/split-lines (slurp file_name))))

;; Reads .csv files
(defn read_csv [file_name] 
  (defn split_handler 
    ([chars] (split_handler chars false "" []))
    ([chars quoted buf res]
      (if (empty? chars)
        res
        (cond 
          (and (= (first chars) ",") (not quoted)) (recur (rest chars) quoted "" (conj res (if (empty? buf) "null" buf)))
          (= (first chars) "\"") (recur (rest chars) (not quoted) buf res)
          :else (recur (rest chars) quoted (str buf (first chars)) res)))))

  (map
    (fn [row] (split_handler (conj (str/split row #"") ",")))
    (str/split-lines (slurp file_name))))

;; Reads .tsv files
(defn read_tsv [file_name] 
  (defn split_handler 
    ([chars] (split_handler chars "" []))
    ([chars buf res]
      (if (empty? chars)
        res
        (if (= (first chars) "\t") 
          (recur (rest chars) "" (conj res (if (empty? buf) "null" buf)))
          (recur (rest chars) (str buf (first chars)) res)))))

  (map
    (fn [row] (split_handler (conj (str/split row #"") "\t")))
    (str/split-lines (slurp file_name))))

;; Sorts the result table
(defn apply_order 
  ([table params] 
    (if (empty? params)
      table
    (if (empty? (params :columns)) 
      (throw (AssertionError. "No columns provided in order by clause")) 
      (apply_order table (params :columns) (params :direction)))))
  ([table columns direction]
    (if (empty? columns)
      (if (= direction "asc")
        table
        (vec (conj (seq (reverse (rest table))) (first table)))) 
      (if (< (.indexOf (first table) (first columns)) 0)
        (throw (AssertionError. (str "Column " (first columns) " was not found")))
        (apply_order (vec (conj (seq (quick_sort (vec (rest table)) (.indexOf (first table) (first columns)))) (first table))) (rest columns) direction)))))

;; Filter rows by uniqueness criteria
(defn apply_distinct
  ([table distinct] (if distinct (apply_distinct table [] []) table))
  ([table hash result] 
    (if (empty? table) 
      (vec result)
      (if (> (.indexOf hash (first (first table))) -1)
        (recur (rest table) hash result)
        (recur (rest table) (conj hash (first (first table))) (conj result (first table)))))))

;; Filter rows by given conditions
(defn apply_where [table conditions]
  (defn run_conditions 
    ([conditions] (run_conditions conditions `()))
    ([conditions stack] 
      (cond
        (empty? conditions) (vec (first stack))
        (= (first conditions) "and") (recur (rest conditions) (conj (rest (rest stack)) (intersect (first stack) (second stack))))
        (= (first conditions) "or") (recur (rest conditions) (conj (rest (rest stack)) (unite (first stack) (second stack))))
        :else (recur (rest conditions) (conj stack (first conditions))))
    )
  )

  (defn filter_condition [condition]
    (try 
      (let [
        comp_ind (.indexOf (first table) (condition :column))
        operation (condition :operation)
        value (condition :value)
        ]
    
        (if (> comp_ind -1)
          (vec (conj (remove nil? 
            (map 
              (fn [row] 
                (if (= operation "<=")
                  (if (<= (Integer/parseInt (nth row comp_ind)) (Integer/parseInt value))
                    row
                    nil)
                  (if-not (= (str (nth row comp_ind)) value) 
                    row 
                    nil)))
              (seq (rest table)))) (first table)))
          (throw (AssertionError. (str "Unknown column: \"" (condition :column) "\"")))))
      (catch Exception e (throw (AssertionError. "Invalid comparison of value and columns types")))))

  (if (empty? conditions)
    (vec table)
    (run_conditions (map (fn [el] (if (map? el) (filter_condition el) el)) conditions))))

;; Filtring the columns in the result table
(defn apply_filter [table columns]
  (defn find_index [table_columns select_columns]
    (if (empty? select_columns)
      `()
          (conj 
            (find_index table_columns (rest select_columns)) 
            (if (> (.indexOf table_columns (first select_columns)) -1)
              (.indexOf table_columns (first select_columns))
              (throw (AssertionError. (str "Unknown column: \"" (first select_columns) "\"")))))))

  (defn run_rows_filter [row column_list] 
    (if (empty? column_list)
      `()
      (conj (run_rows_filter row (rest column_list)) (nth row (first column_list)))))

  (vec (cond 
    (= (first columns) "*") table
    (empty? table) table
    (empty? (find_index (first table) columns)) `()
    :else (let [column_list (vec (find_index (first table) columns))]
      (map (fn [row] (vec (run_rows_filter row column_list))) table)))))

;; Formatting the result table
(defn draw_table [table] 
  (def length_vec (find_max_length table))

  (defn draw_row 
    ([row] (draw_row row length_vec))
    ([row len] 
      (if (empty? row)
        "\n"
        (let [spaces_count (quot (- (first len) (count (first row))) 2)] 
          (str 
            (add_symbols spaces_count " ")
            (first row)
            (add_symbols (if (= (mod (- (first len) (count (first row))) 2) 0) spaces_count (inc spaces_count)) " ")
            "|"
            (draw_row (rest row) (rest len)))))))

  (if (empty? table)
    table
    (conj (vec (conj
      (seq (map 
        (fn [row] (str "|" row)) 
        (map 
          draw_row
          (rest table))))
      (str/replace (str "|" (draw_row (first table))) #"[a-z]| |[1-9]" "_") 
      (str "|" (draw_row (first table)))
      (str (add_symbols (count (draw_row (first table))) "_") "\n")))
      (str/replace (str "|" (draw_row (first table))) #"[a-z]| |[1-9]" "_"))))

;; Getting "order by" parameter
(defn get_order_by [query]
  (if (str/includes? query " order by")
    (hash-map
      :direction 
      (cond
        (str/includes? query " desc") "desc"
        :else "asc")
      :columns 
      (vec
        (map
        (fn [col] (str/trim col))
          (str/split 
            (subs query 
              (+ (.indexOf query " order by ") 9) 
              (cond 
                (str/includes? query " asc") (.indexOf query " asc")
                (str/includes? query " desc") (.indexOf query " desc")
                :else (.indexOf query ";")))
          #","))))
    nil))

;; Getting "distinct" parameter
(defn get_distinct [query]
  (if (str/includes? query "distinct")
    true
    false))

;; Getting "where" conditions
(defn get_where [query]
  (defn create_map [condition]
    (hash-map
      :column (if (str/includes? condition "<=") 
                (str/trim (subs condition 0 (.indexOf condition "<=")))
                (str/trim (subs condition 0 (.indexOf condition "<>")))) 
      :operation (if (str/includes? condition "<=") "<=" "<>") 
      :value (if (str/includes? condition "<=")
                (str/trim (subs condition (+ (.indexOf condition "<=") 2) (count condition)))
                (str/trim (subs condition (+ (.indexOf condition "<>") 2) (count condition))))))

  (defn handle_logic [where_string]
    (if (= where_string "")
      []
      (cond
        (str/includes? where_string " or ") 
          (flatten [(handle_logic (str/trim (subs where_string 0 (.lastIndexOf where_string "or")))) 
            (handle_logic (str/trim (subs where_string (+ (.lastIndexOf where_string "or") 2) (count where_string))))
             "or"])
      
        (str/includes? where_string " and ") 
          (flatten [(handle_logic (str/trim (subs where_string 0 (.lastIndexOf where_string "and")))) 
            (handle_logic (str/trim (subs where_string (+ (.lastIndexOf where_string "and") 3) (count where_string))))
             "and"])
             
        :else [(create_map (str/trim where_string))])))

  (try 
    (vec 
      (handle_logic 
        (if (str/includes? query "where") 
          (str/trim (subs query (+ (.indexOf query "where") 6) (.indexOf query (if-not (nil? (get_order_by query)) " order by" ";"))))
          "")))
    (catch Exception e (throw (AssertionError. "Invalid where syntax")))))

;; Getting column names from the query
(defn get_columns [query]
  (try
    (def column_string 
      (if (get_distinct query) 
        (str/trim (subs query (+ (.indexOf query "distinct") 9) (.indexOf query "from")))
        (str/trim (subs query (+ (.indexOf query "select") 7) (.indexOf query "from")))))
  
    (map (fn [s] (str/trim s)) (str/split column_string #","))
  (catch Exception e (throw (AssertionError. "Invalid columns input")))))

;; Getting table name from the query
(defn get_table [query]
  (try
    (str/trim 
      (subs 
        (str/trim query) (+ (.indexOf (str/trim query) "from") 5) 
        (cond 
          (str/includes? query " where ") (.indexOf (str/trim query) "where")
          (str/includes? query " order by") (.indexOf (str/trim query) " order by")
          :else (.indexOf (str/trim query) ";"))))
    (catch Exception e (throw (AssertionError. "Invalid input")))))

;;Creating a hash-map with query parameters
(defn get_params [query]
  (hash-map 
    :columns (get_columns query), 
    :table_name (get_table query), 
    :distinct (get_distinct query),
    :where (get_where query),
    :order_by (get_order_by query)))

;; Main function
(defn select [query]
  (def params (get_params query))
  (def table (cond 
    (read_file (params :table_name) ["mp-election"]) (read_txt (str (params :table_name) ".txt"))
    (read_file (params :table_name) ["mp-assistants" "map_zal-skl9" "mp-posts_full"]) (read_csv (str (params :table_name) ".csv"))
    (read_file (params :table_name) ["plenary_register_mps-skl9"]) (read_tsv (str (params :table_name) ".tsv"))
    :else (throw (AssertionError. (str "Couldn`t find a table with the name: \"" (params :table_name) "\"")))))

  (println 
    (str/join 
      (draw_table
        (apply_order
          (apply_filter 
            (apply_where 
              (apply_distinct table (params :distinct)) 
            (params :where)) 
          (params :columns))
        (params :order_by))))))

;; CLI
(defn cli [] 
  (let [input (read-line)]
    (select input))
  (recur))

(defn -main []
  ;(print (quick_sort [[3 2 3] [4 6 8] [1 3 6] [2 6 7]] 2))
  (cli)
  ;;(print (apply_order [["field"] ["Ceta"] ["Beta"] ["Alpha"] ["Acronim"]] (hash-map :columns ["field"] :direction "asc")))
  ;;(print (insertion_sort [["field"] ["Ceta"] ["Beta"] ["Alpha"] ["Acronim"]] 0 1))
  ;;(println (get_where "select * from mp-posts where id<>5 and ip<=8000 or mp_id<=5;"))
)

