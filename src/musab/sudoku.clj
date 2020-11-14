(ns musab.sudoku
  (:require [clojure.set    :as cs]
            [clojure.pprint :refer [pprint]]
            [clojure.edn    :as edn]
            [clojure.core.reducers :as r]
            ;; App specific
            [musab.brute-force :refer [solve]])
  (:gen-class))

;; REFERENCES
;; 1) Creator of the constraint propagation algorithm Peter Norig
;;               https://norvig.com/sudoku.html
;; 2) A translation of the algorithm to clojure (used certain parts)
;;              http://www.learningclojure.com/2009/11/sudoku_24.html

(declare assign 
         eliminate 
         check
         format-board
         search
         parse-board-state)

;;*****************************************************************************
;;                                  MAIN
;;*****************************************************************************
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Reading board.edn")

  (def input-board (:input (edn/read-string
                            (slurp "./board.edn"))))

  (println "Input board:")
  (pprint input-board)
  
  (println "Pick solving method: ")
  (println "1) Norvig CP (fast)")
  (println "2) Brute force (> 1000x slower)")
  
  (let [input (read-line)]
    (cond (= input "1")
          (pprint (format-board (search
                                 (parse-board-state input-board))))
          (= input "2")
          (pprint (mapv vec (first (solve input-board))))
          
          :else (println "Invalid selection"))))

;;*****************************************************************************
;;                            HELPER FUNCTIONS
;;*****************************************************************************
(def rows "ABCDEFGHI")
(def cols "123456789")

(defn cross [A, B]
  (for [a A b B] (keyword (str a b))))

;; Squares names that will become the keys in our board state map
(def squares (cross rows cols))

;; units are the groups into which squares are grouped: 
;; rows, columns and subsquares
(def unitlist (map set (concat
                        (for [c cols] (cross rows [c]))
                        (for [r rows] (cross [r] cols))
                        (for [rs (partition 3 rows)
                              cs (partition 3 cols)] (cross rs cs)))))

(def units-for-squares
  (let [starting-map (r/reduce
                      (fn [working-map square-name]
                        (assoc working-map square-name nil))
                      {}
                      squares)]
    (r/reduce (fn
                [m k]
                (assoc m k
                       (filterv
                        #(contains? % k)
                        unitlist)))
              starting-map
              squares)))

(def peers-for-squares
  (let [unit-map units-for-squares]
    (r/reduce (fn [m k]
                (assoc m k (disj (apply cs/union (k unit-map)) k)))
              {}
              squares)))

(defn parse-board-state
  [board-state]
  (let [board-vals (flatten board-state)
        values (atom (r/reduce
                  (fn [m square]
                    (assoc m square (apply sorted-set
                                      (into #{} (range 1 10)))))
                  {}
                  squares))
        list-of-given (for [[square digit]
                            (zipmap squares board-vals)
                            :when ((into #{} (range 1 10)) digit)]
                        [square digit])]
    
    (if (every? (fn [[square digit]] 
                  (assign values square digit)) 
                list-of-given)
      @values
      false)))

(defn assign
  [values square assignment-val]
  (let [elimination-candidates (for [v (square @values)
                                     :when (not (= v assignment-val))] v)]
    (if (every?
         #(eliminate values square %) elimination-candidates)
      @values
      false)))

(defn eliminate [values square val]
  (if (not ((square @values) val)) values ;;if it's already not there nothing to do
      (do
        (swap! values assoc-in [square] (disj (square @values) val)) ;;remove it
        (if (= 0 (count (square @values))) ;;no possibilities left
          false                       ;;fail
          (if (= 1 (count (square @values))) ;; one possibility left
            (let [d2 (first (square @values))
                  square-list (for [s2 (square peers-for-squares)] s2)]
              (if (not (every? #(eliminate values % d2) square-list))
                false
                (check values square val)))
            (check values square val))))))

;;check whether the elimination of a value from a square has caused 
;;contradiction or further assignment possibilities
(defn check [values s d]
  (loop [u (s units-for-squares)] ;;for each row, column, and block associated with square s
    (let [dplaces (for [s (first u) :when ((s @values) d)] s)] ;;how many possible placings of d 

      (if (= (count dplaces) 0) ;;if none then we've failed
        false
        (if (= (count dplaces) 1) ;;if only one, then that has to be the answer

          (if (not (assign values (first dplaces) d)) ;;so we can assign it.
            false
            (if (not (empty? (rest u))) (recur (rest u)) values))
          (if (not (empty? (rest u))) (recur (rest u)) values))))))

(defn search
  [board-state]
  (if board-state
  (if (every? #(= 1 (count (% board-state))) squares)
    board-state       ;; every square only had one value so we found the solution!
    (let [optimal-square
          (second (first (sort     ;; which square has fewest choices?
                          (for [s squares :when (> (count (s board-state)) 1)]
                            [(count (s board-state)),s]))))]
      (let [results (for [d (optimal-square board-state)]
                      (do
                        (search
                         (assign (atom board-state) optimal-square d))))]
        (some identity results))))
    false))

(defn format-board
  [board]
  (let [sorted-values (vals (sort board))
        raw-nums (r/reduce (fn [list s] (conj list (first s)))
                           []
                           sorted-values)]
    (into [] (map vec (partition 9 raw-nums)))))
