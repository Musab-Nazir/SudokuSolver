(ns musab.sudoku-test
  (:require [clojure.test :refer :all]
            [musab.sudoku :refer [solve]]))

(def default-boards
  {:1 {:board [[0 0 0  1 0 6  0 0 0]
               [3 0 7  0 2 0  0 0 0]
               [1 0 0  0 5 0  0 0 0]

               [0 0 9  7 0 8  5 0 0]
               [0 0 0  0 0 0  0 1 0]
               [6 5 0  0 0 0  3 0 0]

               [2 0 0  0 6 0  7 0 9]
               [8 0 0  0 0 0  0 4 0]
               [0 0 0  0 0 9  1 3 0]]
       :solution [[9 4 5   1 8 6   2 7 3]
                  [3 6 7   9 2 4   8 5 1]
                  [1 8 2   3 5 7   9 6 4]

                  [4 1 9   7 3 8   5 2 6]
                  [7 2 3   6 9 5   4 1 8]
                  [6 5 8   4 1 2   3 9 7]

                  [2 3 4   5 6 1   7 8 9]
                  [8 9 1   2 7 3   6 4 5]
                  [5 7 6   8 4 9   1 3 2]]}

   :2 {:board [[5 3 0  0 7 0  0 0 0]
               [6 0 0  1 9 5  0 0 0]
               [0 9 8  0 0 0  0 6 0]

               [8 0 0  0 6 0  0 0 3]
               [4 0 0  8 0 3  0 0 1]
               [7 0 0  0 2 0  0 0 6]

               [0 6 0  0 0 0  2 8 0]
               [0 0 0  4 1 9  0 0 5]
               [0 0 0  0 8 0  0 7 9]]
       :solution [[5 3 4  6 7 8  9 1 2]
                  [6 7 2  1 9 5  3 4 8]
                  [1 9 8  3 4 2  5 6 7]
                  
                  [8 5 9  7 6 1  4 2 3]
                  [4 2 6  8 5 3  7 9 1]
                  [7 1 3  9 2 4  8 5 6]
                  
                  [9 6 1  5 3 7  2 8 4]
                  [2 8 7  4 1 9  6 3 5]
                  [3 4 5  2 8 6  1 7 9]]}})

(deftest a-test
  (testing "Tests two examples."
    (is (=
         (mapv vec (first (solve (get-in default-boards [:1 :board]))))
         (get-in default-boards [:1 :solution])))

    (is (=
         (mapv vec (first (solve (get-in default-boards [:2 :board]))))
         (get-in default-boards [:2 :solution])))))
