(ns minesweeper.board
  (:require [clojure.core :as core]))

(def empty-board (vec (repeat 4 (vec (repeat 4 :E)))))

(defn place-mines [board]
  (let [mine-coords (->> (range (* 4 4))
                         (core/shuffle)
                         (take 4))]
    (loop [b board
           mines mine-coords]
      (if (empty? mines)
        b
        (let [pos (first mines)
              x (quot pos 4)
              y (rem pos 4)]
          (recur (assoc-in b [x y] :M) (rest mines)))))))

(def filled-board (place-mines empty-board))

(defn count-adjacent-mines [board]
  (let [n (count board)
        m (count (first board))
        directions [[-1 -1] [-1 0] [-1 1]
                    [0 -1]         [0 1]
                    [1 -1] [1 0] [1 1]]]
    (for [i (range n)
          j (range m)]
      (if (= (get-in board [i j]) :M)
        :M
        (let [mine-count (->> directions
                              (map (fn [[dx dy]] (get-in board [(+ i dx) (+ j dy)])))
                              (filter #(= % :M))
                              (count))]
          mine-count)))))

(defn final-board [adjacents]
  (->> adjacents
       (partition 4)
       (mapv vec)))

(def board-to-play (final-board (count-adjacent-mines filled-board)))

(def  undisclosed-board (atom (vec (repeat 4 (vec (repeat 4 '[_]))))))