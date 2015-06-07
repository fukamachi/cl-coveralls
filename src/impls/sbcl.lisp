(in-package :cl-user)
(defpackage cl-coveralls.impls.sbcl
  (:nicknames :cl-coveralls.impls)
  (:use :cl)
  (:import-from :cl-coveralls.util
                :get-report-directory)
  (:import-from :lquery
                :$
                :initialize)
  (:import-from :cl-fad
                :list-directory)
  (:export :enable-coverage
           :disable-coverage
           :initialize-coverage
           :finalize-coverage
           :source-path-of-report-file
           :get-coverage-from-report-file))
(in-package :cl-coveralls.impls.sbcl)

(defun enable-coverage ()
  (declaim (optimize sb-cover:store-coverage-data)))

(defun disable-coverage ()
  (declaim (optimize (sb-cover:store-coverage-data 0))))

(defun initialize-coverage ()
  (enable-coverage))

;; returns report files
(defun finalize-coverage ()
  (disable-coverage)
  (let ((report-dir (get-report-directory)))
    (ensure-directories-exist report-dir)
    (let ((*error-output* (make-broadcast-stream)))
      (sb-cover:report report-dir))
    (remove "cover-index.html"
            (fad:list-directory report-dir)
            :key #'file-namestring
            :test #'string=)))

(defun source-path-of-report-file (html)
  (string-right-trim '(#\Newline #\Space)
                     (subseq (aref (lquery:$ (lquery:initialize html) "h3" (text)) 0)
                             17)))

(defun get-coverage-from-report-file (html)
  (mapcar
   (lambda (states)
     (cond
       ((every #'(lambda (el)
                   (or (= el 0)
                       (= el 15)))
               states)
        :null)
       ((find 2 states :test #'=)
        0)
       (t 1)))
   (lquery:$ (lquery:initialize html)
     ".source"
     #'(lambda (lines)
         (map 'list (lambda (line)
                      (lquery:$ line
                        "span"
                        (attr "class")
                        #'(lambda (states)
                            (map 'list (lambda (state) (parse-integer (subseq state 6))) states))))
              lines)))))
