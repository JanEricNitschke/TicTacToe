(def win-conditions [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])


(defn ask []
  (println "Please answer \"y\"[yes] or \"n\"[no]:")
  (.trim (read-line)))

(defn get-valid-answer [question]
  (println question)
  (let [answer (->> (repeatedly ask)
                    (filter #{"y" "n"})
                    (first))]
    (= answer "y")))

(defn swap-player [player]
  (if (= player "X")
    "O"
    "X"))

(defn get-player-input [])

(defn show-board [board]
  (println (str (nth board 0) " | " (nth board 1) " | " (nth board 2)))
  (println "---------")
  (println (str (nth board 3) " | " (nth board 4) " | " (nth board 5)))
  (println "---------")
  (println (str (nth board 6) " | " (nth board 7) " | " (nth board 8))))

(defn player-turn [player board]

  (loop []
    (println (str "Player " player " turn"))
    (show-board board)
    (let [player-input (read-string (read-line))]
      (println player-input)
      (if (and (number? player-input) (<= 0 player-input 8))
        (assoc board player-input player)
        (do
          (println "Invalid input")
          (recur))))))

(defn board-full [board]
  (if (every? #(or (= % "X") (= % "O")) board)
    (do (println "Game drawn!") true)
    false))

(defn won-win-condition [board player win-condition]
  (every? #(= player (nth board %)) win-condition))

(defn game-won [board player]
  (if (some (partial won-win-condition board player) win-conditions)
    (do (println (str "Player " player " wins!")) true)
    false))

(defn play [board]
  (loop [game-board board player "X"]
    (if (or (game-won game-board (swap-player player)) (board-full game-board))
      (show-board board)
      (recur (player-turn player game-board) (swap-player player)))))


(defn -main []
  (play ["0" "1" "2" "3" "4" "5" "6" "7" "8"]))

(-main)
