(in-package :cl-user)
(defpackage cl-coveralls-test
  (:use :cl
        :cl-coveralls
        :prove))
(in-package :cl-coveralls-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-coveralls)' in your Lisp.

(plan nil)

;; blah blah blah.

(finalize)
