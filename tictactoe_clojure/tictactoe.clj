(def win-conditions [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])


(defn swap-player [player]
  (if (= player "X")
    "O"
    "X"))

(defn empty-cells [board]
  (map #(Integer. %) (filter #(not (or (= % "X") (= % "O"))) board)))

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
      (if (and (number? player-input) (<= 0 player-input 8) (#(not (or (= % "X") (= % "O"))) (get board player-input)))
        (assoc board player-input player)
        (do
          (println "Invalid input")
          (recur))))))

(defn board-full [board]
  (if (every? #(or (= % "X") (= % "O")) board)
    (do (println "Game drawn!") true)
    false))

(defn won-win-condition [player board win-condition]
  (every? #(= player (nth board %)) win-condition))

(defn is-win [player board]
  (some (partial won-win-condition player board) win-conditions))

(defn game-won [player board]
  (if (is-win player board)
    (do (println (str "Player " player " wins!")) true)
    false))

(defn random-open-spot [board]
  (let [empty-spaces (empty-cells board)]
    (nth empty-spaces (rand-int (count empty-spaces)))))

(defn ai-turn-easy [player board]
  (let [empty-spaces (empty-cells board)
        random-space (random-open-spot empty-spaces)]
    (assoc board random-space player)))

(defn try-win-move [player board]
  (let [empty-spaces (empty-cells board)]
    (some (fn [space]
            (let [new-board (assoc board space player)]
              (if (is-win player new-board)
                space
                false)))
          empty-spaces)))

(defn ai-turn-medium [player board]
  (let [win-move-spot (try-win-move player board)]
    (if (nil? win-move-spot)
      (ai-turn-easy player board)
      (assoc board win-move-spot player))))

(defn ai-turn-hard [player board]
  (let [win-move-spot (try-win-move player board)]
    (if (nil? win-move-spot)
      (let [block-move-spot (try-win-move (swap-player player) board)]
        (if (nil? block-move-spot)
          (ai-turn-easy player board)
          (assoc board block-move-spot player)))
      (assoc board win-move-spot player))))

(defn ai-best-spot [player board]
  (let [empty-cells (empty-cells board)]
    (cond
      (is-win player board) {:spot -1 :score 1}
      (is-win (swap-player player) board) {:spot -1 :score -1}
      (empty? empty-cells) {:spot -1 :score 0}
      (= (count empty-cells) 9) {:spot (random-open-spot board) :score 0}
      :else
      (reduce
       (fn [best-move cell]
         (let [new-board (assoc board cell player)
               score (- (:score (ai-best-spot (swap-player player) new-board)))]
           (if (>= score (:score best-move))
             {:spot cell :score score}
             best-move)))
       {:spot -1 :score -1}
       empty-cells))))


(defn ai-turn-best [player board]
  (let [spot (:spot (ai-best-spot player board))]  (assoc board spot player)))

(defn ai-turn [player board strength]
  (println (str "AI turn as player " player " with strength " strength))
  (show-board board)
  (let [board-after-ai-move (case strength
                              1 (ai-turn-easy player board)
                              2 (ai-turn-medium player board)
                              3 (ai-turn-hard player board)
                              (ai-turn-best player board))]
    (Thread/sleep 1000)
    board-after-ai-move))

(defn ai-turn? [player X-strength O-strength]
  (if (= player "X")
    (> X-strength 0)
    (> O-strength 0)))

(defn strength-for-player [player X-strength O-strength]
  (if (= player "X")
    X-strength
    O-strength))

(defn play [board X-strength O-strength]
  (loop [player "X" game-board board]
    (if (or (game-won (swap-player player) game-board) (board-full game-board))
      (show-board game-board)
      (let
       [new-board (if (ai-turn? player X-strength O-strength)
                    (ai-turn player game-board (strength-for-player player X-strength O-strength))
                    (player-turn player game-board))]
        (recur (swap-player player) new-board)))))

(defn parse-args
  "Parses command line arguments and returns a map with X-strength and O-strength."
  [args]
  (let [default-x-strength 0
        default-o-strength 0
        x-strength (if (and (>= (count args) 1) (re-matches #"\d+" (nth args 0))) (Integer. (nth args 0)) default-x-strength)
        o-strength (if (and (>= (count args) 2) (re-matches #"\d+" (nth args 1))) (Integer. (nth args 1)) default-o-strength)]
    {:x-strength x-strength
     :o-strength o-strength}))

(defn -main []
  (let [{:keys [x-strength o-strength]} (parse-args *command-line-args*)]
    (play ["0" "1" "2" "3" "4" "5" "6" "7" "8"] x-strength o-strength)))


(-main)
