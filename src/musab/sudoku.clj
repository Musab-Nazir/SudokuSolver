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
(def rows "ABCDEFGHI")
(def cols "123456789")

(defn cross [A, B]
  (for [a A b B] (keyword (str a b))))

;;Squares names that will become the keys in our board state map
(def squares (cross rows cols))

;;units are the groups into which squares are grouped: rows, columns and subsquares
(def unitlist (map set (concat
                         (for [c cols] (cross rows [c]))
                         (for [r rows] (cross [r] cols))
                         (for [rs (partition 3 rows)
                               cs (partition 3 cols)] (cross rs cs)))))

(defn units-for-squares
  []
  (let [starting-map (reduce
                      (fn [working-map square-name]
                        (assoc working-map square-name nil))
                      {}
                      squares)]
    (reduce (fn
              [m k]
              (assoc m k
                     (filterv
                      #(contains? % k)
                      unitlist)))
            starting-map
            squares)))

(defn peers-for-squares
  []
  (let [unit-map (units-for-squares)]
    (reduce (fn [m k]
              (assoc m k (disj (apply cs/union (k unit-map)) k)))
     {}
     squares)))

(defn create-board-map
  []
  (reduce (fn [board-map square-name]
            (assoc board-map square-name (into [] (range 1 10))))
          {} squares))

(defn get-peers-for-square
  [square]
  )

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
  (if (or (nil? row-num) (nil? col-num)) [nil nil]
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
        (into [] box))))

(def box-cords
  {:1 [1 1]
   :2 [1 4]
   :3 [1 7]
   :4 [4 1]
   :5 [4 4]
   :6 [4 7]
   :7 [7 1]
   :8 [7 4]
   :9 [7 7]})

(defn count-row-num
  [row-vals]
  (if (not-any? zero? row-vals) 0 (count (filter (comp not zero?) row-vals))))

(defn get-start-block
  [board-state]
  (let [first-box (count-row-num (get-box-values (:1 box-cords) board-state))
        second-box (count-row-num (get-box-values (:2 box-cords) board-state))
        third-box (count-row-num (get-box-values (:3 box-cords) board-state))
        fourth-box (count-row-num (get-box-values (:4 box-cords) board-state))
        fifth-box (count-row-num (get-box-values (:5 box-cords) board-state))
        six-box (count-row-num (get-box-values (:6 box-cords) board-state))
        seven-box (count-row-num (get-box-values (:7 box-cords) board-state))
        eight-box (count-row-num (get-box-values (:8 box-cords) board-state))
        nine-box (count-row-num (get-box-values (:9 box-cords) board-state))
        max-num (max first-box second-box third-box fourth-box
                     fifth-box six-box seven-box eight-box nine-box)]
    (some (fn [[key val]]
            (and (=
                  (count
                   (filter
                    (comp not zero?)
                    (get-box-values val board-state))) max-num) (key box-cords)))
          box-cords)))

(defn get-zero-in-block
  [board-state]
  (let [[row col] (get-start-block board-state)
        vals (get-box-values [row col] board-state)
        blank-index (first (keep-indexed (fn [idx v] (if (= 0 v) idx)) vals))]
    (cond
      (= blank-index 0) [row col]
      (= blank-index 1) [row (inc col)]
      (= blank-index 2) [row (+ 2 col)]
      (= blank-index 3) [(inc row) col]
      (= blank-index 4) [(inc row) (inc col)]
      (= blank-index 5) [(inc row) (+ 2 col)]
      (= blank-index 6) [(+ 2 row) col]
      (= blank-index 7) [(+ 2 row) (inc col)]
      (= blank-index 8) [(+ 2 row) (+ 2 col)])))

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

(comment (defn solve-old [board-state]
           (let [[row col] (get-first-blank-coords board-state)]
    ;; if no more 0 on the board we have a solution
    ;; else we continue recursion
             (if (or (nil? row) (nil? col))
               (to-array board-state)
               (flatten (mapv #(solve (assoc-in board-state [row col] %))
                              (get-valid-nums [(inc row) (inc col)] board-state)))))))

(defn solve [board-state]
  (let [[row col] (get-zero-in-block board-state)]
    ;; if no more 0 on the board we have a solution
    ;; else we continue recursion
    (if (or (nil? row) (nil? col))
      (to-array board-state)
      (flatten (mapv #(solve (assoc-in board-state [(- row 1) (- col 1)] %))
                     (get-valid-nums [row col] board-state))))))
