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

(defun enable-coverage ()
  (error "Not supported implementation: ~A" (lisp-implementation-type)))
(defun disable-coverage ()
  (error "Not supported implementation: ~A" (lisp-implementation-type)))
(defun initialize-coverage ()
  (error "Not supported implementation: ~A" (lisp-implementation-type)))
(defun finalize-coverage ()
  (error "Not supported implementation: ~A" (lisp-implementation-type)))
(defun source-path-of-report-file (html)
  (declare (ignore html)))
(defun get-coverage-from-report-file (html)
  (declare (ignore html)))
