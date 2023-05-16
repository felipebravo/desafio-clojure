(ns minesweeper.game
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [minesweeper.board :as board]))

(defn start-game []
  (load-file "board.clj")
  (println "OBS: O indice das linhas e das colunas inicia em 0.")
  (println "- Use '(place-flag indice_LINHA indice_COLUNA)' para colocar uma bandeira.")
  (println "- Use '(remove-flag indice_LINHA indice_COLUNA)' para remover uma bandeira.")
  (println "- Use '(explore indice_LINHA indice_COLUNA)' para revelar um indice.")
  (pprint board/undisclosed-board))

(defn explore [row col]  
  (let [index-value (get-in board/board-to-play [row col])]
    (if (= index-value :M)
      (do
        (println "!BOOM! Voce foi derrotado !BOOM!")
        (println "- Use '(start-game)' para reiniciar."))
      (do
        (swap! board/undisclosed-board
               (fn [board]
                 (assoc board row (assoc (board row) col [index-value]))))
        (doseq [row-vec @board/undisclosed-board]
          (println (str/join " " row-vec)))))))

(defn check-win-by-flag [board]
  (let [matched-indices (filter #(and (= (@board %) '[F]) (= (board/board-to-play %) :M)) (range (count @board)))]
    (if (= (count matched-indices) 4)
      (println (count matched-indices))
      (println "Parabéns, você venceu!!!"))))

(defn place-flag [row col]
  (let [updated-row (assoc (get @board/undisclosed-board row) col '[F])
        updated-undisclosed-board (assoc @board/undisclosed-board row updated-row)]
    (swap! board/undisclosed-board (constantly updated-undisclosed-board))
    (doseq [row-vec updated-undisclosed-board]
      (println (str/join " " (map #(cond
                                    (= % :hidden) "_"
                                    (= % :mine) "X"
                                    (= % :flag) "_"
                                    :else %) row-vec))))
    (check-win-by-flag updated-undisclosed-board)))

(defn remove-flag [row col]
  (let [updated-row (update-in (get @board/undisclosed-board row) [col] (constantly '[_]))
        updated-undisclosed-board (assoc @board/undisclosed-board row updated-row)]
    (swap! board/undisclosed-board (constantly updated-undisclosed-board))
    (doseq [row-vec updated-undisclosed-board]
      (println (str/join " " (map #(cond
                                    (= % :hidden) "_"
                                    (= % :mine) "X"
                                    (= % :flag) "_"
                                    :else %) row-vec))))))

