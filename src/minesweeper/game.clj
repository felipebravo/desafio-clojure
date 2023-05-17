(ns minesweeper.game
  (:require [clojure.pprint :refer [pprint]]
            [clojure.string :as str]
            [minesweeper.board :as board]))

(defn formatted-time [time]
  (quot time 1000))

(defrecord GameRecord [record time])

(defonce record (atom (GameRecord. nil nil)))

(defonce best-record (atom nil))

(defn start-game []
  (load-file "board.clj")
  (println "OBS: O indice das linhas e das colunas inicia em 0.")
  (println "- Use '(place-flag indice_LINHA indice_COLUNA)' para colocar uma bandeira.")
  (println "- Use '(remove-flag indice_LINHA indice_COLUNA)' para remover uma bandeira.")
  (println "- Use '(explore indice_LINHA indice_COLUNA)' para revelar um indice.")
  (pprint board/undisclosed-board)
  (reset! record (GameRecord. @board/undisclosed-board (System/currentTimeMillis)))
  (println "Tempo da partida: Iniciado"))

(defn explore [row col]  
  (let [index-value (get-in board/board-to-play [row col])]
    (if (= index-value :M)
      (do
        (println "!BOOM! Voce foi derrotado !BOOM!")
        (println "- Use '(start-game)' para reiniciar.")
        (let [game-time (formatted-time (- (System/currentTimeMillis) (:time @record)))]
          (println "Tempo da partida: "game-time" segundos")))
      (do
        (swap! board/undisclosed-board
               (fn [board]
                 (assoc board row (assoc (board row) col [index-value]))))
        (doseq [row-vec @board/undisclosed-board]
          (println (str/join " " row-vec)))))))

(defn handle-record [] 
  (let [current-time (System/currentTimeMillis) 
        game-time (formatted-time (- current-time (:time @record)))
        best-record (formatted-time (when (:record @record) (:time @record)))] 
        (if (or (nil? best-record) (< game-time best-record))
          (do
            (reset! record (GameRecord. @board/undisclosed-board game-time))
            (println "Parabens, voce venceu! Novo recorde registrado: "game-time" segundos."))
          (do
            (println "Parabens, voce venceu! ! !")))
        (println "Tempo da partida: "game-time" segundos"))
  )

(defn check-win-by-flag [board]
  (let [flattened-board (flatten board)
        flattened-board-to-play (flatten board/board-to-play)
        matched-elements (filter (fn [[x y]] (and (= x 'F) (= y :M)))
                                (map vector flattened-board flattened-board-to-play))]
    (if (= (count matched-elements) 4)
       (handle-record))))

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

