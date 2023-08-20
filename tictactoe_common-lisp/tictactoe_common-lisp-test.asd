#|
  This file is a part of tictactoe_common-lisp project.
|#

(in-package :cl-user)
(defpackage tictactoe_common-lisp-test-asd
  (:use :cl :asdf))
(in-package :tictactoe_common-lisp-test-asd)

(defsystem tictactoe_common-lisp-test
  :author ""
  :license ""
  :depends-on (:tictactoe_common-lisp
               :cl-test-more
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "tictactoe_common-lisp-test"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
