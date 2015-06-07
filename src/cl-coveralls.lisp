(in-package :cl-user)
(defpackage cl-coveralls
  (:nicknames :coveralls)
  (:use :cl)
  (:import-from :cl-coveralls.impls
                :enable-coverage
                :disable-coverage
                :initialize-coverage
                :finalize-coverage
                :source-path-of-report-file
                :get-coverage-from-report-file)
  (:import-from :drakma
                :http-request)
  (:import-from :cl-fad
                :with-open-temporary-file)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :digest-file)
  (:import-from :jsown
                :to-json)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from :alexandria
                :when-let
                :with-gensyms
                :ensure-list)
  (:export :with-coveralls
           :calc-coverage))
(in-package :cl-coveralls)

(defun report-to-coveralls (reports &key dry-run)
  (unless reports
    (return-from report-to-coveralls))

  (let ((json
          (jsown:to-json
           `(:obj
             ("service_job_id" . ,(service-job-id))
             ("service_name" . ,(string-downcase (service-name)))
             ("source_files" . ,(mapcar (lambda (report)
                                          `(:obj ,@report))
                                        reports))))))
    (if dry-run
        (prin1 json)
        (let ((json-file (fad:with-open-temporary-file (out :direction :output :keep t)
                           (write-string json out)
                           (pathname out))))
          (multiple-value-bind (body status)
              (drakma:http-request "https://coveralls.io/api/v1/jobs"
                                   :method :post
                                   :parameters `(("json_file" ,json-file
                                                              :content-type "application/json"
                                                              :filename ,(file-namestring json-file)))
                                   :force-binary t)
            (unless (= status 200)
              (error "An HTTP request failed: ~A" (flex:octets-to-string body :external-format :utf-8))))))))

(defun pathname-in-directory-p (path directory)
  (let ((directory (pathname-directory directory))
        (path (pathname-directory path)))
    (loop for dir1 = (pop directory)
          for dir2 = (pop path)
          if (null dir1)
            do (return t)
          else if (null dir2)
            do (return nil)
          else if (string/= dir1 dir2)
            do (return nil)
          finally
             (return t))))

(defun normalize-exclude-path (root-dir path)
  (probe-file
   (etypecase path
     (string (merge-pathnames (pathname path) root-dir))
     (pathname path))))

(defun get-coverage (fn &key exclude project-dir)
  (unless project-dir
    (error "Project directory is undefined"))
  (let ((root-dir (namestring (probe-file project-dir))))
    (initialize-coverage)
    (loop for file in (uiop:directory-files project-dir)
          when (string= (pathname-type file) "asd")
            do (let ((system-name (pathname-name file)))
                 (if (asdf:component-loaded-p system-name)
                     (asdf:load-system system-name :force t)
                     #+quicklisp (ql:quickload system-name)
                     #-quicklisp (asdf:load-system system-name))))
    (let ((result (unwind-protect (funcall fn)
                    (disable-coverage))))
      (values
       (loop for report-file in (finalize-coverage)
             for source-path = (source-path-of-report-file report-file)
             for normalized-source-path = (cond
                                            ((null source-path))
                                            ((null root-dir) source-path)
                                            ((and (pathname-in-directory-p source-path root-dir)
                                                  (not (find source-path
                                                             (ensure-list exclude)
                                                             :key (lambda (path)
                                                                    (normalize-exclude-path root-dir path))
                                                             :test (lambda (path1 path2)
                                                                     (when path2
                                                                       (setf path1 (merge-pathnames path1 root-dir))
                                                                       (if (uiop:directory-pathname-p path2)
                                                                           (pathname-in-directory-p path1 path2)
                                                                           (equal path1 path2)))))))
                                             (subseq source-path (length root-dir)))
                                            (t nil))
             when normalized-source-path collect
               `(("name" . ,normalized-source-path)
                 ("source-digest" . ,(ironclad:byte-array-to-hex-string
                                      (ironclad:digest-file :md5 source-path)))
                 ("coverage" . ,(get-coverage-from-report-file report-file))))
       result))))

(defun calc-coverage (fn &key project-dir exclude)
  (unless project-dir
    (error ":project-dir is required"))
  (loop for report in (get-coverage fn :exclude exclude :project-dir project-dir)
        for coverage = (cdr (assoc "coverage" report :test #'string=))
        sum (count :null coverage :test-not #'eql) into all
        sum (count 1 coverage) into pass
        finally (return (/ (round (* (/ pass all) 10000)) 100.0))))

(defmacro with-coveralls ((&key exclude dry-run (project-dir '(project-dir))) &body body)
  (with-gensyms (reports result)
    `(if (asdf::getenv "COVERALLS")
         (multiple-value-bind (,reports ,result)
             (get-coverage (lambda () ,@body)
                           :exclude ,exclude :project-dir ,project-dir)
           (report-to-coveralls ,reports :dry-run ,dry-run)
           ,result)
         (progn ,@body))))

(defun service-name ()
  (cond
    ((asdf::getenv "TRAVIS") :travis-ci)
    (t :travis-ci)))

(defun service-job-id (&optional (service-name (service-name)))
  (ecase service-name
    (:travis-ci (asdf::getenv "TRAVIS_JOB_ID"))))

(defun project-dir (&optional (service-name (service-name)))
  (ecase service-name
    (:travis-ci
     (when-let (travis-build-dir (asdf::getenv "TRAVIS_BUILD_DIR"))
       (uiop:ensure-directory-pathname travis-build-dir)))))
