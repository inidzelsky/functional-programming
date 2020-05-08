(ns lab6
    (:use [flatland.ordered.map])
    (:require [clojure.string :as str])
    (:gen-class)
)

;; Appends 2 lists
(defn append [list1 list2]
  (cond
    (empty? list2) list1
    (empty? list1) list2
    :else (recur (conj list1 (first list2)) (rest list2))))

;; Swaps 2 elements of the vector 
(defn swap 
  ([table i1 i2] (if (<= i1 i2) (swap table i1 i2 true) (swap table i2 i1 true)))
  ([table i1 i2 bool] 
    (vec (concat (conj (subvec table 0 i1) (nth table i2)) (conj (subvec table (inc i1) i2) (nth table i1)) (subvec table (inc i2) (count table))))))

;; Deletes the element from the list by index
(defn delete [list index]
  (vec (concat (subvec list 0 index) (if (> (count list) index) (subvec list (inc index) (count list)) []))))

;; Quick sorting algorithm implementation
(defn quick_sort [table column_index]

  ;; Compare mechanisms for strings and ints
  (defn compare_mechanism [row1 row2 column_index]
      (try 
        (> (compare (Integer/parseInt (nth row1 column_index)) (Integer/parseInt (nth row2 column_index))) 0)
      (catch Exception e (> (compare (nth row1 column_index) (nth row2 column_index)) 0))))

  ;; Sorts elements of the list on smaller and larger than chosen
  (defn partiate [table i j]

    ;; Searches for the larger element on the left side
    (defn start [table i]
      (if (and (< i (count table)) (compare_mechanism (nth table 0) (nth table i) column_index))
        (recur table (inc i))
        i))
    
    ;; Searches for the smaller element on the right side
    (defn finish [table j]
      (if (and (> j 0) (compare_mechanism (nth table j) (nth table 0) column_index))
        (recur table (dec j))
        j))
    
    (let [ni (start table i)
          nj (finish table j)]

    ;; Swaps smaller and larger elements if they were found
    (if (< ni nj) 
      (recur (swap table ni nj) (inc ni) (dec nj))
      (hash-map :table table :pointer ni))))

    (if (<= (count table) 1)
      table
      (let [res (partiate table 1 (dec (count table)))]
        (vec 
          (concat
            (quick_sort (subvec (res :table) 1 (res :pointer)) column_index)
            [(first (res :table))]
            (quick_sort (subvec (res :table) (res :pointer) (count (res :table))) column_index))))))
  
;; Unites 2 tables
(defn unite [table1 table2]
  (defn rows_equal_unite [row1 row2] 
    (if (and (empty? row1) (empty? row2))
      true
      (if (= (first row1) (first row2))
        (recur (rest row1) (rest row2))
        false)))

  (defn compare_mechanism_unites [first table]
      (> (.indexOf (map (fn [row] (rows_equal_unite first row)) table) true) -1))

  (if (empty? table1)
    (vec table2)
    (if (compare_mechanism_unites (first table1) table2)
      (recur (rest table1) table2)
      (recur (rest table1) (conj table2 (first table1))))))

;; Intersects 2 tables
(defn intersect 
  ([table1 table2] (intersect table1 table2 []))
  ([table1 table2 acc]
    (defn rows_equal_intersect [row1 row2] 
      (if (and (empty? row1) (empty? row2))
        true
        (if (= (first row1) (first row2))
          (recur (rest row1) (rest row2))
          false)))

    (defn compare_mechanism_intersect [first table]
      (> (.indexOf (map (fn [row] (rows_equal_intersect first row)) table) true) -1))

    (if (empty? table1)
      (vec acc)
      (if (compare_mechanism_intersect (first table1) table2)
        (recur (rest table1) table2 (conj acc (first table1)))
        (recur (rest table1) table2 acc)))))

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

;; Parses table by file type
(defn parse_table [table_name] 
    (vec (remove empty? (cond 
      (read_file table_name ["mp-election"]) (read_txt (str table_name ".txt"))
      (read_file table_name ["mp-assistants" "map_zal-skl9" "mp-posts_full"]) (read_csv (str table_name ".csv"))
      (read_file table_name ["plenary_register_mps-skl9"]) (read_tsv (str table_name ".tsv"))
      :else (throw (AssertionError. (str "Couldn`t find a table with the name: \"" table_name "\"")))))))

;; Concates 2 tables horizontally on the same column value
(defn apply_join [table1 params]
  (if-not (empty? params)
    (let 
      [type (params :type)
      table2 (parse_table (params :table2))
      index1 (.indexOf (first table1) (params :column_name1))
      index2 (.indexOf (first table2) (params :column_name2))]
          
      (defn inner_join [table1 table2 index1 index2 acc]
        (if (empty? table2)
          acc
          (recur table1 (rest table2) index1 index2 
            (append acc 
              (remove nil? 
                (map 
                  (fn [row] 
                    (if (= (nth row index1) (nth (first table2) index2)) 
                      (append row (first table2))  
                      nil)) 
                  table1))))))

      (cond 
        (< index1 0) (throw (AssertionError. (str "Unfound column \"" (params :column_name1) "\"")))
        (< index2 0) (throw (AssertionError. (str "Unfound column \"" (params :column_name2) "\"")))
        (= type "inner") (vec (conj (inner_join (rest table1) (rest table2) index1 index2 []) (concat (first table1) (first table2))))))
    table1))

;; Applyies aggregate functions on column
(defn apply_aggregate [table params]

  ;; Counts average value of the column
  (defn agg_avg
    ([table column_index] 
      (vector [(str "avg(" (nth (first table) column_index) ")")] [(agg_avg (rest table) column_index (hash-map :count (count (rest table)) :res 0))]))

    ([table column_index acc]
      (try
        (if (empty? table)
          (str (/ (float (acc :res)) (acc :count)))
          (if (= (nth (first table) column_index) "null")
            (agg_avg (rest table) column_index (assoc acc :count (dec (acc :count))))
            (agg_avg (rest table) column_index (assoc acc :res (+ (acc :res) (Integer/parseInt (nth (first table) column_index)))))))
        (catch NumberFormatException e (throw (AssertionError. "Can`t apply avg function on string"))))))

  ;; Counts max value of the column
  (defn agg_max
    ([table column_index]
      (try
        (vector [(str "max(" (nth (first table) column_index) ")")] [(agg_max (rest table) column_index (Integer/parseInt (nth (first (rest table)) column_index)))])
        (catch NumberFormatException e (throw (AssertionError. "Can`t apply avg function on string")))))

     ([table column_index acc]
        (try
         (if (empty? table)
           (str acc)
           (if (or (= (nth (first table) column_index) "null") (< (Integer/parseInt (nth (first table) column_index)) acc))
            (agg_max (rest table) column_index acc)
            (agg_max (rest table) column_index (Integer/parseInt (nth (first table) column_index)))))
          (catch NumberFormatException e (throw (AssertionError. "Can`t apply avg function on string"))))))
           

  (let [column_index (.indexOf (first table) (params :column_name))]
    (if (> column_index -1)
      (cond
        (= (params :function) "avg") (agg_avg table column_index)
        (= (params :function) "max") (agg_max table column_index)
        :else (throw (AssertionError. (str "Unknown function " (params :function))))) 
      (throw (AssertionError. (str "Unknown column name " (params :column_name)))))
  ))

;; Sorts the result table
(defn apply_order 
  ([table params] 
    (if (empty? params)
      table
    (if (empty? (params :columns)) 
      (throw (AssertionError. "No columns provided in order by clause")) 
      (apply_order table (params :columns) (params :direction)))))


  ([table columns direction]
    ;; Creates inner vectors and inserts rows with the same value on column_index
    (defn group_map [table column_index]
      (defn run_rows [rows acc]
        (if (empty? rows)
          (vec (vals acc))
          (let [key (nth (first rows) column_index) value (first rows)]
            (if (contains? acc key)
              (recur (rest rows) (assoc acc key (conj (acc key) value)))
              (recur (rest rows) (assoc acc key [value]))))))

        (if (string? (first (first table)))
          (run_rows table (ordered-map))
            (vec (map (fn [t] (group_map t column_index)) table))))

    ;; Sorts every found group in the table
    (defn sort_map [table column_ind]
      (if (string? (first (first table)))
        (quick_sort table column_ind)
        (vec (map (fn [t] (sort_map t column_ind)) table))))
    
    ;; Pulls out all the rows from the groups
    (defn pull_rows 
      ([table row_length] (pull_rows (flatten table) [] row_length []))
      ([table temp_row row_length acc]
        (if (empty? table)
          (seq acc)
          (if (= (count temp_row) row_length)
            (recur (rest table) [(first table)] row_length (conj acc temp_row))
            (recur (rest table) (conj temp_row (first table)) row_length acc)))))

    (let [column_index (.indexOf (first table) (first columns))] 
      (if (> column_index -1)
        (let [sorted_table (sort_map (vec (rest table)) column_index)]
          (if (empty? (rest columns))
            (if (= direction "asc")
              (vec (conj (pull_rows sorted_table (count (first table))) (first table)))
              (vec (conj (reverse (pull_rows sorted_table (count (first table)))) (first table))))
            (apply_order (vec (conj (seq (group_map sorted_table column_index)) (first table))) (rest columns) direction)))
        (throw (AssertionError. (str "Column " (first columns) " was not found")))))))

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

  ;; Defines the order of operations with the table
  (defn run_conditions 
    ([conditions] (run_conditions conditions `()))
    ([conditions stack] 
      (cond
        (empty? conditions) (vec (first stack))
        (= (first conditions) "and") (recur (rest conditions) (conj (seq (rest (rest stack))) (intersect (first stack) (second stack))))
        (= (first conditions) "or") (recur (rest conditions) (conj (seq (rest (rest stack))) (unite (first stack) (second stack))))
        :else (recur (rest conditions) (conj (seq stack) (first conditions))))))

  ;; Filters table with the given condition
  (defn filter_condition [condition]
    (try 
      (let [
        comp_ind (.indexOf (first table) (condition :column))
        operation (condition :operation)
        value (condition :value)
        ]
    
        (if (> comp_ind -1)
          (vec (conj
            (filter
              (fn [row] 
                (if (= operation "<=")
                  (<= (Integer/parseInt (nth row comp_ind)) (Integer/parseInt value))
                  (not (= (str (nth row comp_ind)) value))))
              (seq (rest table))) (first table)))
          (throw (AssertionError. (str "Unknown column: \"" (condition :column) "\"")))))
      (catch Exception e (throw (AssertionError. "Invalid comparison of value and columns types")))))

  (if (empty? conditions)
    (vec table)
    (run_conditions (map (fn [el] (if (map? el) (filter_condition el) el)) conditions))))

;; Filtring the columns in the result table
(defn apply_filter [table columns]

  ;; Finds indexes of given columns
  (defn find_index [table_columns select_columns]
    (if (empty? select_columns)
      `()
      (conj 
        (find_index table_columns (rest select_columns)) 
          (if (> (.indexOf table_columns  (first select_columns)) -1)
            (.indexOf table_columns (first select_columns))
            (throw (AssertionError. (str "Unknown column: \"" (first select_columns) "\"")))))))

  ;; Takes a row and filters it with column_list indexes
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

;; Formatting the result table for printing
(defn format_table [table] 

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

  ;; Vector with max length of each column
  (def length_vec (find_max_length table))

  ;; Formats row
  (defn format_row 
    ([row] (format_row row length_vec))
    ([row len] 
      (if (empty? row)
        "\n"
        (let [spaces_count (quot (- (first len) (count (first row))) 2)] 
          (str 
            (add_symbols (if (= (mod (- (first len) (count (first row))) 2) 0) spaces_count (inc spaces_count)) " ")
            (first row)
            (add_symbols spaces_count " ")
            "|"
            (format_row (rest row) (rest len)))))))

  (if (empty? table)
    table
    (conj (vec (conj
      (seq (map 
        (fn [row] (str "|" row)) 
        (map 
          format_row
          (rest table))))
      (str/replace (str "|" (format_row (first table))) #"[^|\n\t_]" "_") 
      (str "|" (format_row (first table)))
      (str (add_symbols (count (format_row (first table))) "_") "\n")))
      (str/replace (str "|" (format_row (first table))) #"[^|\n\t_]" "_"))))

;; Getting "join ..." parameters
(defn get_join [query]
  (if (str/includes? query " join ")
    (try
      (let [subquery (cond 
                        (str/includes? query " where ") (subs query (+ (.indexOf query " join ") 6) (.indexOf query " where "))
                        (str/includes? query " order by ") (subs query (+ (.indexOf query " join ") 6) (.indexOf query "order by "))
                        (str/includes? query ";") (subs query (+ (.indexOf query " join ") 6) (.lastIndexOf query ";")))]
     (hash-map 
        :type (cond 
                (str/includes? query " inner join ") "inner"
                (str/includes? query " full outer join ") "outer"
                (str/includes? query " right join ") "right")
        :table2 (str/trim (subs subquery 0 (.indexOf subquery " on ")))
        :column_name1 (str/trim (subs (subs subquery (+ (.indexOf subquery " on ") 4) (.indexOf subquery " = ")) (inc (.indexOf (subs subquery (+ (.indexOf subquery " on ") 4) (.indexOf subquery " = ")) "."))))
        :column_name2 (str/trim (subs (subs subquery (+ (.indexOf subquery " = ") 3) (count subquery)) (inc (.indexOf (str/trim (subs subquery (+ (.indexOf subquery " = ") 3) (count subquery))) "."))))))
        (catch Error e (throw (AssertionError. "Invalid join format"))))
      {})
    )

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
  ;
  ; Creates a hash-map with parameters
  (defn create_map [condition]
    (hash-map
      :column (if (str/includes? condition "<=") 
                (str/trim (subs condition 0 (.indexOf condition "<=")))
                (str/trim (subs condition 0 (.indexOf condition "<>")))) 
      :operation (if (str/includes? condition "<=") "<=" "<>") 
      :value (if (str/includes? condition "<=")
                (str/trim (subs condition (+ (.indexOf condition "<=") 2) (count condition)))
                (str/trim (subs condition (+ (.indexOf condition "<>") 2) (count condition))))))

  ;; Creates parameters vector from the string
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
          (cond 
            (str/includes? query " order by ") (subs query (+ (.indexOf query " where ") 7) (.indexOf query " order by "))
            :else (subs query (+ (.indexOf query " where ") 7) (.indexOf query ";")))
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
        query 
        (+ (.indexOf query "from") 5) 
        (cond 
          (str/includes? query " inner join ") (.indexOf query " inner join ")
          (str/includes? query " full outer join ") (.indexOf query " full outer join ")
          (str/includes? query " right join ") (.indexOf query " right join ")
          (str/includes? query " where ") (.indexOf query "where")
          (str/includes? query " order by ") (.indexOf query " order by ")
          :else (.indexOf query ";"))))
    (catch Exception e (throw (AssertionError. "Invalid input")))))

;; Getting aggregate function and column name from the query
(defn get_aggregate [query]
  (cond 
    (str/includes? query "avg(") 
      (hash-map 
        :function "avg" 
        :column_name (subs query (+ (.indexOf query "avg(") 4) (.indexOf query ")")))
    (str/includes? query "max(")
      (hash-map 
        :function "max" 
        :column_name (subs query (+ (.indexOf query "max(") 4) (.indexOf query ")")))
        :else {}))

;;Creating a hash-map with query parameters
(defn get_params [query]
  (hash-map 
    :columns (get_columns query), 
    :table_name (get_table query), 
    :distinct (get_distinct query),
    :where (get_where query),
    :order_by (get_order_by query)
    :aggregate (get_aggregate query)
    :join (get_join query)))

;; Main function
(defn select [query]
  (def params (get_params query))

  (println 
    (str/join 
      (format_table
        (if (empty? (params :aggregate))
          (apply_order
            (apply_filter 
              (apply_where 
                (apply_distinct
                  (apply_join (parse_table (params :table_name)) (params :join))
                 (params :distinct)) 
              (params :where)) 
            (params :columns))
          (params :order_by))
          (apply_aggregate (parse_table (params :table_name)) (params :aggregate)))))))

;; CLI
(defn cli [] 
  (let [input (read-line)]
    (select input))
    (recur))

(defn -main []
  (cli))

