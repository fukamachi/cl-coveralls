(in-package :cl-user)
(defpackage cl-coveralls.util
  (:use :cl)
  (:export :get-report-directory))
(in-package :cl-coveralls.util)

(defun generate-random-string ()
  (format nil "~36R" (random (expt 36 #-gcl 8 #+gcl 5))))

(defun get-report-directory ()
  (let ((tmpdir (pathname-directory (uiop:default-temporary-directory))))
    (rplacd (last tmpdir) (list (generate-random-string)))
    (make-pathname :directory tmpdir)))
