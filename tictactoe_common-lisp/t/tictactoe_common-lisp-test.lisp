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


(in-suite :tictactoe.common-lisp)
