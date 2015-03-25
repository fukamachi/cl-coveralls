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
                :with-gensyms
                :ensure-list)
  (:export :with-coveralls))
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

(defun normalize-exclude-path (root-dir path)
  (let ((path
          (etypecase path
            (string (merge-pathnames (pathname path) root-dir))
            (pathname path))))
    (cond
      ((probe-file path) path)
      ((uiop:file-pathname-p path)
       (setf path (uiop:ensure-directory-pathname path))
       (if (probe-file path)
           path
           nil))
      (t nil))))

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

(defmacro with-coveralls ((&key exclude) &body body)
  (with-gensyms (report-file source-path normalized-source-path project-dir root-dir file system-name g-exclude)
    `(if (asdf::getenv "COVERALLS")
         (let* ((,project-dir (project-dir))
                (,g-exclude (ensure-list ,exclude))
                (,root-dir (and ,project-dir
                                (namestring (probe-file ,project-dir)))))
           (initialize-coverage)
           (loop for ,file in (uiop:directory-files ,project-dir)
                 when (string= (pathname-type ,file) "asd")
                   do (let ((,system-name (pathname-name ,file)))
                        (if (asdf:component-loaded-p ,system-name)
                            (asdf:load-system ,system-name :force t)
                            #+quicklisp (ql:quickload ,system-name)
                            #-quicklisp (asdf:load-system ,system-name))))
           (prog1 (unwind-protect (progn ,@body)
                    (disable-coverage))
             (report-to-coveralls
              (loop for ,report-file in (finalize-coverage)
                    for ,source-path = (source-path-of-report-file ,report-file)
                    for ,normalized-source-path = (cond
                                                    ((null ,source-path))
                                                    ((null ,root-dir)
                                                     ,source-path)
                                                    ((and (pathname-in-directory-p ,source-path ,root-dir)
                                                          (not (find ,source-path
                                                                     ,g-exclude
                                                                     :key #'normalize-exclude-path
                                                                     :test #'pathname-in-directory-p)))
                                                     (subseq ,source-path (length ,root-dir)))
                                                    (t nil))
                    when ,normalized-source-path collect
                      `(("name" . ,,normalized-source-path)
                        ("source_digest" . ,(ironclad:byte-array-to-hex-string
                                             (ironclad:digest-file :md5 ,source-path)))
                        ("coverage" . ,(get-coverage-from-report-file ,report-file)))))))
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
    (:travis-ci (uiop:ensure-directory-pathname (asdf::getenv "TRAVIS_BUILD_DIR")))))
