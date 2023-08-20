(in-package :cl-user)
(defpackage :tictactoe_common-lisp-test
  (:use :common-lisp
        :fiveam
        :tictactoe_common-lisp))
(in-package :tictactoe_common-lisp-test)

(def-suite :tictactoe.common-lisp
    :description "Test the common lisp version of tictactoe")

(def-suite :other-player
    :in :tictactoe.common-lisp
    :description "Test the other-player function")
(in-suite :tictactoe.common-lisp)


(test :swap-from-x
      (let ((result (other-player tictactoe_common-lisp::'x)))
	(is (eql tictactoe_common-lisp::'o result))))
