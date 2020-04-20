(ns lab5
    (:require [clojure.string :as str])
    (:gen-class)
)

;; Testing

(defn check_columns_amount [table] 
  (def ideal (count (first table)))
  ((fn [table] 
    (if (empty? table)
      true
      (if (= ideal (count (first table)))
        (recur (rest table))
        false))
  ) table)
)

;; Helpers

;; Finds in every column the length of the longest element
(defn find_max_length [table]
  (defn compare_vectors [vec1 vec2]
    (if (empty? vec1)
      (seq vec2)
      (if (empty? vec2) 
        (seq vec1) 
        (conj (compare_vectors (rest vec1) (rest vec2)) (if (> (first vec1) (first vec2)) (first vec1) (first vec2)))))
  )

  (defn run_rows_max [table result] 
    (if (empty? table)
      result
      (recur (rest table) (compare_vectors result (map (fn [element] (count element)) (first table)))))
  )

  (vec (run_rows_max table []))
)

(defn add_symbols [num symb] 
  (if (> num 0)
    (str symb (add_symbols (dec num) symb))
    "")
)

;; Realisation

;; Defines whether files vector includes table
(defn read_file [table_name files_vec]
  (if (empty? files_vec) 
    false 
    (if (= (first files_vec) table_name) 
      true
      (recur table_name (rest files_vec))))
)

;; Reads .txt files
(defn read_txt [file_name]
  (defn split_handler [row]
    (if (> (count (str/split row #"\t")) 1)
      (str/split row #"\t")
      (str/split row #"  "))
  )

  (map 
    (fn [row] (split_handler row))
    (str/split-lines (slurp file_name)))
)

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
          :else (recur (rest chars) quoted (str buf (first chars)) res))))  
  )

  (map
    (fn [row] (split_handler (conj (str/split row #"") ",")))
    (str/split-lines (slurp file_name)))
)

;; Reads .tsv files
(defn read_tsv [file_name] 
  (defn split_handler 
    ([chars] (split_handler chars "" []))
    ([chars buf res]
      (if (empty? chars)
        res
        (if (= (first chars) "\t") 
          (recur (rest chars) "" (conj res (if (empty? buf) "null" buf)))
          (recur (rest chars) (str buf (first chars)) res))))  
  )

  (map
    (fn [row] (split_handler (conj (str/split row #"") "\t")))
    (str/split-lines (slurp file_name)))
)

(defn apply_distinct
  ([table distinct] (if distinct (apply_distinct table [] []) table))
  ([table hash result] 
    (if (empty? table) 
      (vec result)
      (if (> (.indexOf hash (first (first table))) -1)
        (recur (rest table) hash result)
        (recur (rest table) (conj hash (first (first table))) (conj result (first table))))))
)

(defn apply_where [table where]
  (if (empty? where)
    (vec table)
    (let [
      comp_ind (.indexOf (first table) (where :column))
      operation (where :operation)
      value (Integer/parseInt (where :value))
      ]
    
      (if (> comp_ind -1)
        (vec (conj (remove nil? 
          (map 
            (fn [row] 
              (if (= operation "<=")
                (if (<= (Integer/parseInt (nth row comp_ind)) value)
                  row
                  nil)
                (if-not (= (Integer/parseInt (nth row comp_ind)) value) 
                  row 
                  nil)))
            (seq (rest table)))) (first table)))
        (throw (AssertionError. (str "Unknown column: \"" (where :column) "\""))))))
)

;; Filtring the columns in the result table
(defn apply_filter [table columns]
  (defn find_index [table_columns select_columns]
    (if (empty? select_columns)
      `()
          (conj 
            (find_index table_columns (rest select_columns)) 
            (if (> (.indexOf table_columns (first select_columns)) -1)
              (.indexOf table_columns (first select_columns))
              (throw (AssertionError. (str "Unknown column: \"" (first select_columns) "\""))))))
  )

  (defn run_rows_filter [row column_list] 
    (if (empty? column_list)
      `()
      (conj (run_rows_filter row (rest column_list)) (nth row (first column_list))))
  )

  (vec (cond 
    (= (first columns) "*") table
    (empty? table) table
    (empty? (find_index (first table) columns)) `()
    :else (let [column_list (vec (find_index (first table) columns))]
      (map (fn [row] (vec (run_rows_filter row column_list))) table))))
)

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
            (draw_row (rest row) (rest len)))))) 
  )

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
      (str/replace (str "|" (draw_row (first table))) #"[a-z]| |[1-9]" "_")))
)

;; Getting distinct parameter
(defn get_distinct [query]
  (if (str/includes? query "distinct")
    true
    false)
)

;; Getting "where" conditions
(defn get_where [query]
  (try
    (def where_string 
      (if (str/includes? query "where") 
        (subs query (+ (.indexOf query "where") 5) (count query))
        ""))

    (if-not (empty? where_string)
      (hash-map
        :column (if (str/includes? where_string "<=") 
                  (str/trim (subs where_string 0 (.indexOf where_string "<=")))
                  (str/trim (subs where_string 0 (.indexOf where_string "<>")))) 
        :operation (if (str/includes? where_string "<=") "<=" "<>") 
        :value (if (str/includes? where_string "<=")
                  (str/trim (subs where_string (+ (.indexOf where_string "<=") 2) (- (count where_string) 1)))
                  (str/trim (subs where_string (+ (.indexOf where_string "<>") 2) (- (count where_string) 1)))))
      {})
  (catch Exception e (throw (AssertionError. "Invalid input after \"where\" context"))))
)

;; Getting column names from the query
(defn get_columns [query]
  (try
    (def column_string 
      (if (get_distinct query) 
        (str/trim (subs query (+ (.indexOf query "distinct") 9) (.indexOf query "from")))
        (str/trim (subs query (+ (.indexOf query "select") 7) (.indexOf query "from")))))
  
    (map (fn [s] (str/trim s)) (str/split column_string #","))
  (catch Exception e (throw (AssertionError. "Invalid columns input"))))
)

;; Getting table name from the query
(defn get_table [query]
  (try
    (str/trim 
      (subs 
        (str/trim query) (+ (.indexOf (str/trim query) "from") 5) 
        (if (str/includes? query "where") 
          (.indexOf (str/trim query) "where")
          (.indexOf (str/trim query) ";"))))
    (catch Exception e (throw (AssertionError. "Invalid input"))))
)

;;Creating a hash-map with query parameters
(defn get_params [query] 
  (hash-map 
    :columns (get_columns query), 
    :table_name (get_table query), 
    :distinct (get_distinct query),
    :where (get_where query))
)

;; Main function
(defn select [query]
  (def params (get_params query))
  (def table (cond 
    (read_file (params :table_name) ["mp-election"]) (read_txt (str (params :table_name) ".txt"))
    (read_file (params :table_name) ["mp-assistants" "map_zal-skl9" "mp-posts_full"]) (read_csv (str (params :table_name) ".csv"))
    (read_file (params :table_name) ["plenary_register_mps-skl9"]) (read_tsv (str (params :table_name) ".tsv"))
    :else (throw (AssertionError. (str "Couldn`t find a table with the name: \"" (params :table_name) "\"")))))

  (println 
    (str/join (draw_table 
        (apply_filter 
          (apply_where 
            (apply_distinct table (params :distinct)) 
          (params :where)) 
        (params :columns)))))
)


(defn cli [] 
  (let [input (read-line)]
    (select input))
  (recur)
)

(defn -main []
  (cli)
)

