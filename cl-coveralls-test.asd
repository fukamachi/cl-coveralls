#|
  This file is a part of cl-coveralls project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-coveralls-test-asd
  (:use :cl :asdf))
(in-package :cl-coveralls-test-asd)

(defsystem cl-coveralls-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:cl-coveralls
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-coveralls"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
