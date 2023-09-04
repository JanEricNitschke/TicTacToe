;;;; Commandline tictactoe in common lisp.

(in-package :cl-user)
(defpackage :tictactoe_common-lisp
  (:use :common-lisp)
  (:export
   :play
   :x
   :o))

(in-package :tictactoe_common-lisp)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
                      ,@(when doc (list doc))))

(define-constant +straights+ '(
			    ; Rows
			    (0 1 2) (3 4 5) (6 7 8)
			    ; Cols
			    (0 3 6) (1 4 7) (2 5 8)
			    ; Diagonals
			    (0 4 8) (2 4 6))
  "List of straights that constitute a game win.")

;; Generate an empty board as it should be when the
;; game starts.
(defun generate-board ()
  (loop repeat 9 collect nil))

;; Pretty-print the board.
;; Replace nils with their index.
(defun show-board (board)
  (let ((line-separator "----------------"))
    (format t "~a~%" line-separator)
    (dotimes (row 3)
      (dotimes (col 3)
	(let ((index (+ (* 3 row) col)))
	  (format t "| ~a |" (or (nth index board) index))))
      (format t "~%~a~%" line-separator))))

;; Check if the whole board is filled.
(defun board-filled-p (board)
  (dolist (value board)
    (unless value
      (return-from board-filled-p nil)))
  t)


;; Check if a player has fulfilled a straight.
;; Returns a list containing the
(defun check-straight (straight player board)
  (let ((result (list 0 ())))
    (dolist (spot straight)
      (cond
	((eql (elt board spot) player) (incf (first result)))
	((eql (elt board spot) nil) (push spot (second result)))))
    (setf (second result)(reverse (second result)))
    result))

;; Check if the given player has won the game.
(defun player-won-p (board player)
  (dolist (straight +straights+)
    (let ((result (check-straight straight player board)))
      (when (= (first result) 3)
	(return-from player-won-p t))))
  nil)

;; Swap the current player between "x" and "o".
(defun other-player (current-player)
  (if (eql current-player 'x)
      'o
      'x))

;; Read input from the user given a prompt.
(defun prompt-read (prompt)
  (format *query-io* "~a: ~%" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

;; Check if the given move can be performed.
;; If so, perform that move.
(defun fix-spot-p (spot player board)
  (when (or (< spot 0) (> spot 8))
    (format t "ERROR: Spot has to be in range [0-8]!~%")
    (return-from fix-spot-p nil))
  (when (elt board spot)
    (format t "ERROR Spot ~a is already occupied!~%" spot)
    (return-from fix-spot-p nil))
  (setf (elt board spot) player)
  t)

;; Perform a player turn.
(defun player-turn (player board)
  (format t "Player ~a turn~%." player)
  (show-board board)
  (loop
     (let ((user-input (parse-integer (prompt-read "Where to make your next move? [0-8]") :junk-allowed t)))
       (when (and user-input (fix-spot-p user-input player board))
	 (return)))))


;; Ask whether the player should be played by the AI
(defun ai-player-p (player)
  (y-or-n-p (format nil "Should player ~a be played by AI?" player)))


;; Get the AI strength for the given player
(defun get-ai-strength (player)
  (format t "AI strength settings:~%")
  (format t "1: Easy~%")
  (format t "2: Medium~%")
  (format t "3: Hard~%")
  (format t "4: Impossible~%")
  (loop
     (let ((user-input (parse-integer
			(prompt-read
			 (format nil "How strong should the AI for player ~a be? [1 - 4]" player))
			 :junk-allowed t)))
       (when (and user-input (and (> user-input 0) (< user-input 5)))
	 (return user-input)))))

;; Get AI settings for the given player
(defun ai-player-settings (player)
  (if (ai-player-p player)
      (list t (get-ai-strength player))
      (list nil nil)))

;; Checks if the current turn should be performed by the AI
(defun get-turn-settings (player x-ai o-ai)
  (if (eql player 'x)
      x-ai
      o-ai))

;; Get a random element from the list
(defun get-random-element (lst)
  (nth (random (length lst) (make-random-state t)) lst))

;; Check if a spot is open on the board
(defun legal-p (spot board)
  (null (elt board spot)))

;; Get a list of all empty spots
(defun empty-spots (board)
  (loop for i from 0 to (1- (length board))
     when (legal-p i board)
     collect i))

;; Get a list (spot -1) where spot represents any valid open spot
(defun random-move (board)
  (list (get-random-element (empty-spots board)) nil))

;; Get list of winning move spots
(defun get-winning-moves (board player)
  (let ((winning-spots '()))
    (dolist (straight +straights+)
      (let ((result (check-straight straight player board)))
	(when (and (= (first result) 2) (= 1 (list-length (second result))))
	  (push (first (second result)) winning-spots))))
    (nreverse winning-spots)))

;; Get winning or random move
(defun win-move (board player)
  (let ((winning-moves (get-winning-moves board player)))
	(if winning-moves
	    (list (get-random-element winning-moves) nil)
	    (random-move board))))

;; Get winning, blocking or random move
(defun win-block-move (board player)
  (let ((winning-moves (get-winning-moves board player)))
    (when winning-moves
      (return-from win-block-move (list (get-random-element winning-moves) nil))))
  (let ((blocking-moves (get-winning-moves board (other-player player))))
	(when blocking-moves
	  (return-from win-block-move (list (get-random-element blocking-moves) nil))))
  (random-move board))


;; Get list of optimal moves
(defun get-best-moves (board player)
  (when (player-won-p board player)
    (return-from get-best-moves (list nil 1)))
  (when (player-won-p board (other-player player))
    (return-from get-best-moves (list nil -1)))
  (when (board-filled-p board)
    (return-from get-best-moves (list nil 0)))
  (let ((empties (empty-spots board)))
    (when (= 9 (list-length empties))
      (return-from get-best-moves (list (loop :for n :below 9 :collect n) 0)))
    (let ((best-moves (list nil -2)))
      (dolist (spot empties)
	(setf (elt board spot) player)
	(let ((current-score (second (get-best-moves board (other-player player)))))
	  (cond
	    ((= (- current-score) (second best-moves)) (push spot (first best-moves)))
	    ((> (- current-score) (second best-moves))  (setf best-moves (list (list spot) (- current-score))))))
	(setf (elt board spot) nil))
      best-moves)))

;; Get an optimal move
(defun min-max (board player)
  (destructuring-bind (spots score) (get-best-moves board player)
    (list (get-random-element spots) score)))

;; Perform an AI turn
(defun ai-turn (player board ai-strength)
  (format t "Player ~a turn~%." player)
  (show-board board)
  (let ((spot (cond
		((eql 1 ai-strength) (random-move board))
		((eql 2 ai-strength) (win-move board player))
		((eql 3 ai-strength) (win-block-move board player))
		((eql 4 ai-strength) (min-max board player))
		(t (random-move board)))))
    (setf (elt board (first spot)) player))
  (sleep 1))

;; Ask the user for the AI strength

;; Play a game of tictactoe by performing alternating player turns
;; and after each checking whether the game is over by draw or win.
(defun play ()
  (let ((game-board (generate-board))
	(current-player 'x)
	(x-ai-settings (ai-player-settings 'x))
	(o-ai-settings (ai-player-settings 'o)))
    (loop
       (destructuring-bind (ai-turn-now ai-strength) (get-turn-settings current-player x-ai-settings o-ai-settings)
	 (if ai-turn-now
	     (ai-turn current-player game-board ai-strength)
	     (player-turn current-player game-board))
	 (when (player-won-p game-board current-player)
	   (format t "Player ~a wins the game!~%" current-player)
	   (return))
	 (when (board-filled-p game-board)
	   (format t "Match Drawn!~%")
	   (return))
	 (setf current-player (other-player current-player))))
    (show-board game-board)))


(defun recompile ()
  (load "D:/Programming/Projects/TicTacToe/TicTacToe/tictactoe_common-lisp/src/tictactoe_common-lisp.lisp"))

(defun replay ()
  (recompile)
  (play))
