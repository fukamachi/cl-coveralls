(in-package :cl-user)
(defpackage cl-coveralls.impls.ccl
  (:nicknames :cl-coveralls.impls)
  (:use :cl)
  (:import-from :cl-coveralls.util
                :get-report-directory)
  (:import-from :alexandria
                :starts-with-subseq
                :ends-with-subseq
                :read-file-into-string)
  (:import-from :split-sequence
                :split-sequence)
  (:export :enable-coverage
           :disable-coverage
           :initialize-coverage
           :finalize-coverage
           :source-path-of-report-file
           :get-coverage-from-report-file))
(in-package :cl-coveralls.impls.ccl)

(defun enable-coverage ()
  (setq ccl:*compile-code-coverage* t))

(defun disable-coverage ()
  (setq ccl:*compile-code-coverage* nil))

(defun initialize-coverage ()
  (enable-coverage))

(defun finalize-coverage ()
  (disable-coverage)
  (let ((report-dir (get-report-directory)))
    (ensure-directories-exist report-dir)
    (handler-case (ccl:report-coverage (merge-pathnames #P"report.html" report-dir))
      (error (e)
        (warn (princ-to-string e))))
    (remove-if-not
     (lambda (file)
       (and (not (string= (file-namestring file) "report.html"))
            (string= (pathname-type file) "html")))
     (uiop:directory-files report-dir))))

(defun source-path-of-report-file (html)
  (let ((path
          (string-right-trim '(#\Newline #\Space)
                             (subseq (aref (lquery:$ (lquery:initialize html) "h3" (text)) 0)
                                     17))))
    (when (ends-with-subseq ".newest" path)
      (setf path (subseq path (- (length path) (length ".newest")))))
    (if (starts-with-subseq "home:" path)
        (let ((path-components
                (split-sequence #\; (subseq path 5))))
          (merge-pathnames
           (make-pathname
            :name (last path-components)
            :directory (cons :relative (butlast path-components)))
           (user-homedir-pathname)))
        (let ((path-components
                (split-sequence #\; path)))
          (make-pathname
           :name (last path-components)
           :directory (cons :absolute (butlast path-components)))))))

(defun get-coverage-from-report-file (html)
  (labels ((get-lines (html)
             (let ((sources (lquery:$ (lquery:initialize html)
                              ".source code"
                              (serialize))))
               (remove ""
                       (loop for code across sources
                             append (ppcre:split "<span class=\"line\">\\d+?</span>"
                                                 (ppcre:regex-replace "(?s)^<code>(.+)</code>$" code "\\1")))
                       :test #'string=)))
           (get-section-end-lines (html)
             (map 'list
                  (lambda (code)
                    (let ((lines (lquery:$ code ".line" (text))))
                      (parse-integer (aref lines (1- (length lines))))))
                  (lquery:$ (lquery:initialize html) ".source code")))
           (js-file (html)
             (make-pathname :type "js" :defaults html))
           (read-source-coverage (html)
             (let* ((js-file (js-file html))
                    (js (read-file-into-string js-file))
                    (source-coverage
                      (nth-value 1
                                 (ppcre:scan-to-strings "(?s)var SourceCoverage = \\[(.+?), 'end'\\];" js))))
               (unless source-coverage
                 (error "Invalid JS file: '~A'" js-file))
               (map 'simple-vector #'parse-integer (ppcre:split ",\\s*" (aref source-coverage 0)))))
           (empty-line-p (line)
             (null (ppcre:scan "\\S" (ppcre:regex-replace-all "<.+?>" line ""))))
           (state-to-covered-num (state)
             (case state
               (:null :null)
               (1 0)
               (otherwise 1))))
    (let ((state (list :null))
          (section-ends (get-section-end-lines html))
          (source-coverage (read-source-coverage html)))
      (loop for line in (get-lines html)
            for tags = (ppcre:all-matches-as-strings "(<.+?>)" line)
            for num from 1
            collect
            (loop with line-states = nil
                  with prev-state = (car state)
                  for tag in tags
                  if (starts-with-subseq "</" tag)
                    do (push (pop state) line-states)
                  else do
                    (let ((new-state
                            (aref source-coverage
                                  (parse-integer (aref (nth-value 1 (ppcre:scan-to-strings "<span id=\"f0s(\\d+)\">" tag)) 0)))))
                      (push new-state line-states)
                      (push new-state state))
                  finally
                     (return
                       (cond
                         ((or (empty-line-p line)
                              (null tags))
                          (state-to-covered-num prev-state))
                         ((find 1 line-states) 0)
                         (t 1))))
            ;; reset the stack of states
            if (find num section-ends :test #'=)
              do (setq state (list :null))))))
