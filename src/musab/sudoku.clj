(ns musab.sudoku
  (:require [clojure.set    :as cs]
            [clojure.pprint :refer [pprint]]
            [clojure.edn    :as edn])
  (:gen-class))

(declare solve)

;;*****************************************************************************
;;                                  MAIN
;;*****************************************************************************
(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Reading board.edn")

  (def input-board (edn/read-string
                    (slurp "./board.edn")))

  (print "Input board: \n")

  (pprint (input-board :input))

  (print "Solving...This may take a couple of minutes \n")

  (pprint (mapv vec (first (solve (input-board :input))))))

;;*****************************************************************************
;;                            HELPER FUNCTIONS
;;*****************************************************************************
(defn get-possibilities-from-vec
  "Given a row vector, it will return the numbers from the possible values 
   that have not appeared in that row"
  [vec]
  (filterv (fn [num]
             (not (some #{num} vec)))
           (into [] (range 1 10))))

(defn get-box-values
  "Gets a flat vector for the box associated with the coord provided"
  [[row-num col-num] board-state]
  (let [row-range (cond
                    (<= row-num 3) (range 0 3)
                    (<= row-num 6) (range 3 6)
                    (<= row-num 9) (range 6 9))
        col-range (cond
                    ;; Due to how subvec works the col ranges have n+1
                    ;; as the last num in the range compared to rows
                    (<= col-num 3) (range 0 4)
                    (<= col-num 6) (range 3 7)
                    (<= col-num 9) (range 6 10))
        box (reduce
             (fn [x row-index]
               (concat x (subvec
                          (nth board-state row-index)
                          (first col-range)
                          (last col-range))))
             []
             row-range)]
    (into [] box)))

(defn get-valid-nums
  "Given a row and col number returns a set of valid nums 
   for that position if that position is free else returns 0"
  [[row-num col-num] board-state]
  (let [row (nth board-state (- row-num 1))
        col (into []
                  (mapv
                   (fn [row]
                     (nth row (- col-num 1)))
                   board-state))
        box-vals (get-box-values [row-num col-num] board-state)
        current-num (nth row (- col-num 1))
        row-set (into #{} (get-possibilities-from-vec row))
        col-set (into #{} (get-possibilities-from-vec col))
        box-set (into #{} (get-possibilities-from-vec box-vals))]
    (if (= current-num 0)
      (apply sorted-set (cs/intersection row-set col-set box-set))
      #{})))

(defn possible-number?
  "Checks if a provided number n is possible given a row and col position.
   Uses get-valid-nums internally"
  [[row-num col-num] n board-state]
  (if (some #{n} (get-valid-nums [row-num col-num] board-state)) true false))

(defn get-first-blank-coords
  "Get the coordinates of the first position on the board that is 0"
  [board-state]
  (first
   (remove nil?
           (map (fn [x]
                  (if (= (get-in board-state x) 0) x))
                (for [row (range 9)
                      col (range 9)]
                  [row col])))))

(defn solve [board-state]
  (let [[row col] (get-first-blank-coords board-state)]
    ;; if no more 0 on the board we have a solution
    ;; else we continue recursion
    (if (or (nil? row) (nil? col))
      (to-array board-state)
      (flatten (mapv #(solve (assoc-in board-state [row col] %))
            (get-valid-nums [(inc row) (inc col)] board-state))))))
