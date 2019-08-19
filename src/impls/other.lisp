(in-package :cl-user)
(defpackage cl-coveralls.impls.other
  (:nicknames :cl-coveralls.impls)
  (:use :cl)
  (:export :enable-coverage
           :disable-coverage
           :initialize-coverage
           :finalize-coverage
           :report-files
           :source-path-of-report-file
           :get-coverage-from-report-file))
(in-package :cl-coveralls.impls.other)

(defun not-supported ()
  (error "Not supported implementation: ~A ~A"
         (lisp-implementation-type)
         (lisp-implementation-version)))

(defun enable-coverage () (not-supported))
(defun disable-coverage () (not-supported))
(defun initialize-coverage () (not-supported))
(defun finalize-coverage () (not-supported))
(defun report-files () (not-supported))
(defun source-path-of-report-file (html)
  (declare (ignore html)))
(defun get-coverage-from-report-file (html)
  (declare (ignore html)))
