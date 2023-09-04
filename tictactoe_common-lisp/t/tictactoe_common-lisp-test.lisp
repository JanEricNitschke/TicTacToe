(in-package :cl-user)
(defpackage :tictactoe_common-lisp-test
  (:use :common-lisp
        :fiveam
        :tictactoe_common-lisp))
(in-package :tictactoe_common-lisp-test)

(defmacro setup-suite (name)
  "Shorthand for setting up the individual functions suites"
  `(progn
     (def-suite ,name
	 :in :tictactoe.common-lisp
	 :description ,(format nil "Test the ~a function" name))
     (in-suite ,name)))

(def-suite :tictactoe.common-lisp
    :description "Test the common lisp version of tictactoe")

(setup-suite :other-player)

(test :swap-from-x
      (let ((result (tictactoe_common-lisp::other-player 'x)))
	(is (eql 'o result))))

(test :swap-from-o
      (let ((result (tictactoe_common-lisp::other-player 'o)))
	(is (eql 'x result))))

(setup-suite :generate-board)

(test :generates-empty-board
      (is (equal
	   (list nil nil nil nil nil nil nil nil nil)
	   (tictactoe_common-lisp::generate-board))))


(setup-suite :check-straight)

(test :straight-fulfilled
      (is (equal
	   (list 3 nil)
	   (tictactoe_common-lisp::check-straight
	    (list 0 1 8)
	    'x
	    (list 'x 'x nil nil nil nil nil nil 'x)))))

(test :straight-open
      (is (equal
	   (list 0 (list 0 1 2))
	   (tictactoe_common-lisp::check-straight
	    (list 0 1 2)
	    'x
	    (list nil nil nil nil nil nil nil nil 'x)))))

(test :straight-partial
      (is (equal
	   (list 1 (list 4))
	   (tictactoe_common-lisp::check-straight
	    (list 0 4 8)
	    'o
	    (list 'o nil nil nil nil nil nil nil 'x)))))


(setup-suite :player-won-p)

(test :player-win-row
      (let ((board (list 'x 'x 'x nil nil nil nil nil nil)))
	(is (tictactoe_common-lisp::player-won-p
	     board
	     'x))
	(is (not (tictactoe_common-lisp::player-won-p
		  board
		  'o))))
      (let ((board (list nil nil nil 'o 'o 'o nil nil nil)))
	(is (tictactoe_common-lisp::player-won-p
	     board
	     'o))
	(is (not (tictactoe_common-lisp::player-won-p
		  board
		  'x))))
      (let ((board (list nil nil nil nil nil nil 'x 'x 'x)))
	(is (tictactoe_common-lisp::player-won-p
	     board
	     'x))
	(is (not (tictactoe_common-lisp::player-won-p
		  board
		  'o)))))

(test :player-win-col
      (let ((board (list 'x nil nil 'x nil nil 'x nil nil)))
	(is (tictactoe_common-lisp::player-won-p
	     board
	     'x))
	(is (not (tictactoe_common-lisp::player-won-p
		  board
		  'o))))
      (let ((board (list nil 'o nil nil 'o nil nil 'o nil)))
	(is (tictactoe_common-lisp::player-won-p
	     board
	     'o))
	(is (not (tictactoe_common-lisp::player-won-p
		  board
		  'x))))
      (let ((board (list nil nil 'x nil nil 'x nil nil 'x)))
	(is (tictactoe_common-lisp::player-won-p
	     board
	     'x))
	(is (not (tictactoe_common-lisp::player-won-p
		  board
		  'o)))))

(test :player-win-diag
      (let ((board (list 'x nil nil nil 'x nil nil nil 'x)))
	(is (tictactoe_common-lisp::player-won-p
	     board
	     'x))
	(is (not (tictactoe_common-lisp::player-won-p
		  board
		  'o))))
      (let ((board (list nil nil 'o nil 'o nil 'o nil nil)))
	(is (tictactoe_common-lisp::player-won-p
	     board
	     'o))
	(is (not (tictactoe_common-lisp::player-won-p
		  board
		  'x)))))

(setup-suite :board-filled-p)

(test :board-not-filled
      (is (not (tictactoe_common-lisp::board-filled-p (list nil nil 'o nil 'o nil 'o nil nil)))))

(test :board-filled
      (is (tictactoe_common-lisp::board-filled-p (list 'o 'x 'o 'x 'x 'o 'o 'x 'o))))


(setup-suite :fix-spot-p)

(test :spot-out-of-range
      (let ((board (list nil nil 'o nil 'o nil 'o nil nil)))
	(is (not (tictactoe_common-lisp::fix-spot-p -2 'x board)))
	(is (equal (list nil nil 'o nil 'o nil 'o nil nil) board))
	(is (not (tictactoe_common-lisp::fix-spot-p 14 'o board)))
	(is (equal (list nil nil 'o nil 'o nil 'o nil nil) board))))

(test :spot-occupied
      (let ((board (list nil nil 'o nil 'o nil 'o nil nil)))
	(is (not (tictactoe_common-lisp::fix-spot-p 2 'o board)))
	(is (equal (list nil nil 'o nil 'o nil 'o nil nil) board))))

(test :spot-valid
      (let ((board (list nil nil 'o nil 'o nil 'o nil nil)))
	(is (tictactoe_common-lisp::fix-spot-p 0 'o board))
	(is (equal (list 'o nil 'o nil 'o nil 'o nil nil) board))))


(setup-suite :get-turn-settings)

(test :x-turn
      (is (tictactoe_common-lisp::get-turn-settings 'x t nil))
      (is (tictactoe_common-lisp::get-turn-settings 'x t t))
      (is ( not (tictactoe_common-lisp::get-turn-settings 'x nil nil)))
      (is ( not (tictactoe_common-lisp::get-turn-settings 'x nil t))))

(test :o-turn
      (is (tictactoe_common-lisp::get-turn-settings 'o nil t))
      (is (tictactoe_common-lisp::get-turn-settings 'o t t))
      (is (not (tictactoe_common-lisp::get-turn-settings 'o nil nil)))
      (is (not (tictactoe_common-lisp::get-turn-settings 'o t nil))))

(setup-suite :empty-spots)

(test :get-empty-spots
      (is (equal (list 0 1 2 3 4 5 6 7 8) (tictactoe_common-lisp::empty-spots (list nil nil nil nil nil nil nil nil nil))))
      (is (equal (list 0 1 3 4 7 8) (tictactoe_common-lisp::empty-spots (list nil nil 'x nil nil 'o 'x nil nil))))
      (is (equal () (tictactoe_common-lisp::empty-spots (list 'x 'o 'x 'x 'x 'o 'x 'o 'o)))))

(setup-suite :get-random-element)

(test :is-list-element
      (let ((my-list (list 1 2 3 19 -5)))
	(dotimes (_n_ 100)
	  (is (member (tictactoe_common-lisp::get-random-element my-list) my-list)))))

(setup-suite :legal-p)

(test :is-legal
      (is (tictactoe_common-lisp::legal-p 0 (list nil nil nil nil nil nil nil nil nil nil)))
      (is (tictactoe_common-lisp::legal-p 1 (list nil nil nil nil nil nil nil nil nil nil)))
      (is (tictactoe_common-lisp::legal-p 3 (list nil nil nil nil nil nil nil nil nil nil)))
      (is (tictactoe_common-lisp::legal-p 8 (list 'o 'x 'x 'o 'x 'o 'o 'o nil))))

(test :not-legal
      (is (not (tictactoe_common-lisp::legal-p 0 (list 'o 'x 'x 'o 'x 'o 'o 'o 'o nil))))
      (is (not (tictactoe_common-lisp::legal-p 3 (list 'o 'x 'x 'o 'x 'o 'o 'o 'o nil))))
      (is (not (tictactoe_common-lisp::legal-p 5 (list 'o 'x 'x 'o 'x 'o 'o 'o 'o nil))))
      (is (not (tictactoe_common-lisp::legal-p 7 (list 'o 'x 'x 'o 'x 'o 'o 'o 'o nil)))))

(setup-suite :random-move)

(test :is-valid-move
      (dotimes (_n_ 100)
	(let* ((board (list nil nil 'o 'o 'x nil 'x nil nil)) (move (tictactoe_common-lisp::random-move board)))
	  (is (member (first move) (list 0 1 5 7 8))))))

(setup-suite :get-winning-moves)

(defun same-elements (l1 l2)
  (and (subsetp l1 l2) (subsetp l2 l1)))

(test :winning-move-finds-row
      (let ((moves (tictactoe_common-lisp::get-winning-moves (list 'o 'x 'o 'x 'o nil 'x 'x nil) 'x)))
	(is (same-elements moves (list 8)))))

(test :winning-move-finds-col
      (let ((moves (tictactoe_common-lisp::get-winning-moves (list 'o 'x nil nil nil nil 'o 'x 'x) 'o)))
	(is (same-elements moves (list 3)))))

(test :winning-move-finds-diag
      (let ((moves (tictactoe_common-lisp::get-winning-moves (list 'o nil nil nil 'o nil nil nil nil) 'o)))
	(is (same-elements moves (list 8)))))

(test :winning-move-finds-antidiag
      (let ((moves (tictactoe_common-lisp::get-winning-moves (list nil nil nil nil 'x nil 'x nil nil) 'x)))
	(is (same-elements moves (list 2)))))

(test :winning-move-finds-multiple
      (let ((moves (tictactoe_common-lisp::get-winning-moves (list 'x nil 'x nil 'x nil nil nil nil) 'x)))
	(is (same-elements  moves (list 1 6 8)))))

(test :winning-move-handles-none
      (let ((moves (tictactoe_common-lisp::get-winning-moves (list nil nil nil nil nil nil nil nil nil) 'x)))
	(is (equal  moves nil))))

(setup-suite :win-move)

(test :win-move-finds-row
      (let ((move (tictactoe_common-lisp::win-move (list 'o 'x 'o 'x 'o nil 'x 'x nil) 'x)))
	(is (equal move (list 8 nil)))))

(test :win-move-finds-col
      (let ((move (tictactoe_common-lisp::win-move (list 'o 'x nil nil nil nil 'o 'x 'x) 'o)))
	(is (equal move (list 3 nil)))))

(test :win-move-finds-diag
      (let ((move (tictactoe_common-lisp::win-move (list 'o nil nil nil 'o nil nil nil nil) 'o)))
	(is (equal move (list 8 nil)))))

(test :win-move-finds-antidiag
      (let ((move (tictactoe_common-lisp::win-move (list nil nil nil nil 'x nil 'x nil nil) 'x)))
	(is (equal move (list 2 nil)))))

(test :win-move-finds-multiple
      (let ((move (tictactoe_common-lisp::win-move (list 'x nil 'x nil 'x nil nil nil nil) 'x)))
	(is (equal (second move) nil))
	(is (member (first move) (list 1 6 8)))))

(test :win-move-handles-none
      (let ((move (tictactoe_common-lisp::win-move (list nil nil nil nil nil nil nil nil nil) 'x)))
	(is (equal (second move) nil))
	(is (member (first move) (list 0 1 2 3 4 5 6 7 8)))))

(setup-suite :win-block-move)

(test :win-block-move-finds-win
      (let ((move (tictactoe_common-lisp::win-block-move (list 'x 'x nil 'o 'o nil nil nil nil) 'x)))
	(is (equal move (list 2 nil)))))

(test :win-block-move-blocks-win
      (let ((move (tictactoe_common-lisp::win-block-move (list 'x NIL nil 'o 'o nil nil nil nil) 'x)))
	(is (equal move (list 5 nil)))))

(setup-suite :get-best-moves)

(test :get-best-moves-takes-open-spot
      (let ((moves (tictactoe_common-lisp::get-best-moves (list 'x 'x nil 'o 'x 'o 'x 'o 'o) 'x)))
	(is (equal  moves (list (list 2) 1)))))

(test :get-best-moves-takes-blocks-win
      (let ((moves (tictactoe_common-lisp::get-best-moves (list 'o 'o 'x 'x nil 'o nil 'o 'x) 'x)))
	(is (equal  moves (list (list 4) 0)))))

(test :get-best-moves-takes-takes-win
      (let ((moves (tictactoe_common-lisp::get-best-moves (list 'o 'o 'x 'x nil nil nil 'o 'x) 'o)))
	(is (equal  moves (list (list 4) 1)))))

(test :get-best-moves-finds-best
      (let ((moves (tictactoe_common-lisp::get-best-moves (list 'x nil nil nil nil nil nil nil nil) 'o)))
	(is (equal  moves (list (list 4) 0)))))

(test :get-best-moves-finds-multiple
      (let ((moves (tictactoe_common-lisp::get-best-moves (list 'x nil 'x nil 'x nil nil nil nil) 'x)))
	(is (equal (second moves) 1))
	;; 1 6 8 win immediately.
	;; All other spots win on the next turn.
	(is (same-elements (first moves) (list 1 3 5 6 7 8)))))

(setup-suite :min-max)

(test :min-max-takes-open-spot
      (let ((move (tictactoe_common-lisp::min-max (list 'x 'x nil 'o 'x 'o 'x 'o 'o) 'x)))
	(is (equal  move (list 2 1)))))

(test :min-max-takes-blocks-win
      (let ((move (tictactoe_common-lisp::min-max (list 'o 'o 'x 'x nil 'o nil 'o 'x) 'x)))
	(is (equal  move (list 4 0)))))

(test :min-max-takes-takes-win
      (let ((move (tictactoe_common-lisp::min-max (list 'o 'o 'x 'x nil nil nil 'o 'x) 'o)))
	(is (equal  move (list 4 1)))))

(test :min-max-finds-best
      (let ((move (tictactoe_common-lisp::min-max (list 'x nil nil nil nil nil nil nil nil) 'o)))
	(is (equal  move (list 4 0)))))

(test :min-max-finds-multiple
      (let ((move (tictactoe_common-lisp::min-max (list 'x nil 'x nil 'x nil nil nil nil) 'x)))
	(is (equal (second move) 1))
	;; 1 6 8 win immediately.
	;; All other spots win on the next turn.
	(is (member (first move) (list 1 3 5 6 7 8)))))

(in-suite :tictactoe.common-lisp)
