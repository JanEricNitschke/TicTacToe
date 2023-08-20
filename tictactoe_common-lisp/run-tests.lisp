#-quicklisp
(let ((quicklisp-init "C:/lispstick/quicklisp/setup.lisp"))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
(ql:quickload :fiveam)
(load "tictactoe_common-lisp.asd")
(ql:quickload :tictactoe_common-lisp)
(load "tictactoe_common-lisp-test.asd") ;; <-- where all the FiveAM tests are written.
(ql:quickload :tictactoe_common-lisp-test)
(in-package :tictactoe_common-lisp-test)

(run!)
