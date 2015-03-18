(in-package :cl-user)
(defpackage cl-coveralls.util
  (:use :cl)
  (:import-from :cl-fad
                :get-default-temporary-directory
                :generate-random-string)
  (:export :get-report-directory))
(in-package :cl-coveralls.util)

(defun get-report-directory ()
  (let ((tmpdir (pathname-directory (fad::get-default-temporary-directory))))
    (rplacd (last tmpdir) (list (fad::generate-random-string)))
    (make-pathname :directory tmpdir)))
