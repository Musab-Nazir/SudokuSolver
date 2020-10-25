(ns musab.sudoku
  (:require [clojure.set :as cs])
  (:gen-class))

;; Sample board I'm using for dev purposes
(def board 
  [[5 3 0  0 7 0  0 0 0]
   [6 0 0  1 9 5  0 0 0]
   [0 9 8  0 0 0  0 6 0]
   
   [8 0 0  0 6 0  0 0 3]
   [4 0 0  8 0 3  0 0 1]
   [7 0 0  0 2 0  0 0 6]
   
   [0 6 0  0 0 0  2 8 0]
   [0 0 0  4 1 9  0 0 5]
   [0 0 0  0 8 0  0 7 9]])

;; Solution to the dev example board
#_(def solution
    [[5 3 4  6 7 8  9 1 2]
     [6 7 2  1 9 5  3 4 8]
     [1 9 8  3 4 2  5 6 7]
     
     [8 5 9  7 6 1  4 2 3]
     [4 2 6  8 5 3  7 9 1]
     [7 1 3  9 2 4  8 5 6]
     
     [9 6 1  5 3 7  2 8 4]
     [2 8 7  4 1 9  6 3 5]
     [3 4 5  2 8 6  1 7 9]])

;; Valid list of nums in a sudoku game
(def valid-nums 
  (into [] (range 1 10)))

;; Given a row vector, it will return the numbers from the possible values
;; that have not appeared in that row
(defn get-possibilities-from-vec
  [vec]
  (filterv (fn [num]
             (not (some #{num} vec)))
           valid-nums))

;; Gets a flat vector for the box associated with the coord provided
(defn get-box-values
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

;; Given a row and col number returns a set of valid nums 
;; for that position if that position is free else returns 0
(defn get-valid-nums
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
      0)))

;; Checks if a provided number n is possible given a row and col position
;; Uses get-valid-nums internally
(defn possible-number?
  [[row-num col-num] n board-state]
  (if (some #{n} (get-valid-nums [row-num col-num] board-state)) true false))

;; Get the coordinates of the first position on the board that is 0
(defn get-first-blank-coords
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
    (map #(solve (assoc-in board-state [row col] %))
         (get-valid-nums [(inc row) (inc col)] board-state))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello  World!"))
