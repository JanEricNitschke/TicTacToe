(in-package :cl-user)
(defpackage tictactoe_common-lisp-asd
  (:use :cl :asdf))
(in-package :tictactoe_common-lisp-asd)

(defsystem tictactoe_common-lisp
  :version "0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "tictactoe_common-lisp"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op tictactoe_common-lisp-test))))
