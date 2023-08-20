; (in-package #:tictactoe.common-lisp)

(defconstant +straights+ '(
			    ; Rows
			    (0 1 2) (3 4 5) (6 7 8)
			    ; Cols
			    (0 3 6) (1 4 7) (2 5 8)
			    ; Diagonals
			    (0 4 8) (2 4 6))
  "List of straights that constitute a game win.")

(defun generate-board ()
  (loop repeat 9 collect nil))

(defun get-board-spot (n board)
  (nth (1- n) board))

(defun show-board (board)
  (let ((line-separator "----------------"))
    (format t "~a~%" line-separator)
    (dotimes (row 3)
      (dotimes (col 3)
	(let ((index (+ (* 3 row) col)))
	  (format t "| ~a |" (or (nth index board) index))))
      (format t "~%~a~%" line-separator))))

(defun board-filled-p (board)
  (dolist (value board)
    (unless value
      (return-from board-filled-p nil)))
  t)

(defun check-straight (straight player board)
  (let ((result (list 0 ())))
    (dolist (spot straight)
      (cond
	((eql (elt board spot) player) (incf (first result)))
	(spot (push spot (second result)))))
    result))

(defun player-won-p (board player)
  (dolist (straight +straights+)
    (let ((result (check-straight straight player board)))
      (when (= (first result) 3)
	(return-from player-won-p t))))
  nil)

(defun other-player (current-player)
  (if (eql current-player 'x)
      'o
      'x))


(defun prompt-read (prompt)
  (format *query-io* "~a: ~%" prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun fix-spot (spot player board)
  (when (or (< spot 0) (> spot 8))
    (format t "ERROR: Spot has to be in range [0-8]!~%")
    (return-from fix-spot nil))
  (when (elt board spot)
    (format t "ERROR Spot ~a is already occupied!~%" spot)
    (return-from fix-spot nil))
  (setf (elt board spot) player)
  t)

(defun player-turn (player board)
  (format t "Player ~a turn~%." player)
  (show-board board)
  (loop
       (let ((user-input (parse-integer (prompt-read "Where to make your next move? [0-8]") :junk-allowed t)))
	 (when (and user-input (fix-spot user-input player board))
	   (return)))))

(defun play ()
  (let ((game-board (generate-board)) (current-player 'x))
    (loop
       (player-turn current-player game-board)
       (when (player-won-p game-board current-player)
	 (format t "Player ~a wins the game!~%" current-player)
	 (return))
       (when (board-filled-p game-board)
	 (format t "Match Drawn!~%")
	 (return))
       (setf current-player (other-player current-player)))
    (show-board game-board)))


(defun recompile ()
  (load "D:\\Programming\\Projects\\TicTacToe\\TicTacToe\\tictactoe_common-lisp\\tictactoe_common-lisp.lisp"))

(defun replay ()
  (recompile)
  (play))
