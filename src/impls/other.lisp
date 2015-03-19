(in-package :cl-user)
(defpackage cl-coveralls.impls.other
  (:nicknames :cl-coveralls.impls)
  (:use :cl)
  (:export :enable-coverage
           :disable-coverage
           :initialize-coverage
           :finalize-coverage
           :source-path-of-report-file
           :get-coverage-from-report-file))
(in-package :cl-coveralls.impls.other)
