#|
  This file is a part of cl-coveralls project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-coveralls-asd
  (:use :cl :asdf))
(in-package :cl-coveralls-asd)

(defsystem cl-coveralls
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (#+sbcl :sb-cover
               :ironclad
               :lquery
               :jsown
               :drakma
               :cl-fad
               :cl-ppcre
               :flexi-streams
               :alexandria
               :split-sequence)
  :components ((:module "src"
                :components
                ((:file "cl-coveralls" :depends-on ("impls"))
                 (:module "impls"
                  :depends-on ("util")
                  :components
                  (#+sbcl (:file "sbcl")
                   #+(and ccl-1.4
                          (not :ccl-1.10)) (:file "ccl")
                   #-(or sbcl
                         (and ccl-1.4
                              (not :ccl-1.10))) (:file "other")))
                 (:file "util"))))
  :description "Coverage tracker for Coveralls"
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
  :in-order-to ((test-op (test-op cl-coveralls-test))))
