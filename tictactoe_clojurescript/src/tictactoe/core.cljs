(ns tictactoe.core
  (:require [cljs.reader :as reader]))


(def win-conditions [[0 1 2] [3 4 5] [6 7 8] [0 3 6] [1 4 7] [2 5 8] [0 4 8] [2 4 6]])


(defn swap-player [player]
  (if (= player "X")
    "O"
    "X"))

(defn empty-cells [board]
  (map #(js/parseInt %) (filter #(not (or (= % "X") (= % "O"))) board)))

(defn show-board [board]
  (str (nth board 0) " | " (nth board 1) " | " (nth board 2) "\n"
       "---------\n"
       (nth board 3) " | " (nth board 4) " | " (nth board 5) "\n"
       "---------\n"
       (nth board 6) " | " (nth board 7) " | " (nth board 8)))


(defn player-turn [player board]
  (loop []
    (js/alert (str "Player " player " turn" "\n" (show-board board)))
    (let [player-input (reader/read-string (js/prompt "Enter a number between 0 and 8"))]
      (if (and (number? player-input) (<= 0 player-input 8) (#(not (or (= % "X") (= % "O"))) (get board player-input)))
        (assoc board player-input player)
        (do
          (js/alert "Invalid input")
          (recur))))))

(defn board-full [board]
  (if (every? #(or (= % "X") (= % "O")) board)
    (do (js/alert (str "Game drawn!" "\n" (show-board board))) true)
    false))

(defn won-win-condition [player board win-condition]
  (every? #(= player (nth board %)) win-condition))

(defn is-win [player board]
  (some (partial won-win-condition player board) win-conditions))

(defn game-won [player board]
  (if (is-win player board)
    (do (js/alert (str "Player " player " wins!" "\n" (show-board board))) true)
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
  (js/alert (str "AI turn as player " player " with strength " strength "." "\n" (show-board board)))
  (let [board-after-ai-move (case strength
                              1 (ai-turn-easy player board)
                              2 (ai-turn-medium player board)
                              3 (ai-turn-hard player board)
                              (ai-turn-best player board))]
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
      :default
      (let
       [new-board (if (ai-turn? player X-strength O-strength)
                    (ai-turn player game-board (strength-for-player player X-strength O-strength))
                    (player-turn player game-board))]
        (recur (swap-player player) new-board)))))

(defn ai-options []
  (str "AI options:" "\n"
       "0: Human" "\n"
       "1: Easy" "\n"
       "2: Medium" "\n"
       "3: Hard" "\n"
       "4: Impossible"))


(defn -main []
  (play
   ["0" "1" "2" "3" "4" "5" "6" "7" "8"]
   (reader/read-string (js/prompt (ai-options)))
   (reader/read-string (js/prompt (ai-options)))))


(-main)
