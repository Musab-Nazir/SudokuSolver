(ns musab.sudoku
  (:require [clojure.set :as cs])
  (:gen-class))

;; Sample board I'm using for dev purposes
(def board 
  [[5 3 0 0 7 0 0 0 0]
   [6 0 0 1 9 5 0 0 0]
   [0 9 8 0 0 0 0 6 0]
   [8 0 0 0 6 0 0 0 3]
   [4 0 0 8 0 3 0 0 1]
   [7 0 0 0 2 0 0 0 6]
   [0 6 0 0 0 0 2 8 0]
   [0 0 0 4 1 9 0 0 5]
   [0 0 0 0 8 0 0 7 9]])

;; Valid list of nums in a sudoku game
(def possible-nums 
  (into [] (range 1 10)))

;; Given a row vector, it will return the numbers from the possible values
;; that have not appeared in that row
(defn get-possibilities-from-vec
  [vec]
  (filterv (fn [num]
             (not (some #{num} vec)))
           possible-nums))

;; Given a row and col number returns a set of valid nums 
;; for that position if that position is free else returns 0
(defn take-guess
  [row-num col-num]
  (let [row (nth board (- row-num 1))
        col (into [] 
                  (mapv 
                   (fn [row] 
                     (nth row (- col-num 1))) 
                   board))
        current-num (nth row (- col-num 1))
        row-set (into #{} (get-possibilities-from-vec row))
        col-set (into #{} (get-possibilities-from-vec col))]
    (if (= current-num 0)
      (apply sorted-set (cs/intersection row-set col-set))
      0)))

;; Since each row/col can only have unique 1-9 values the sum needs to be exactly 45
(defn check-vec
  [row]
  (if (= (apply + row) 45)
    true
    false))

;; Flattens a 2D vec for a 3x3 box and checks if it is valid.
;; The same rules apply as a regular row/col i.e unique 1-9 values
(defn check-box
  [box]
  (let [flattened (into [] (flatten box))]
    (print flattened)
    (check-vec flattened)))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello  World!"))

;; Solution to the dev example board
#_(def solution
  [[5 3 4 6 7 8 9 1 2]
   [6 7 2 1 9 5 3 4 8]
   [1 9 8 3 4 2 5 6 7]
   [8 5 9 7 6 1 4 2 3]
   [4 2 6 8 5 3 7 9 1]
   [7 1 3 9 2 4 8 5 6]
   [9 6 1 5 3 7 2 8 4]
   [2 8 7 4 1 9 6 3 5]
   [3 4 5 2 8 6 1 7 9]])
