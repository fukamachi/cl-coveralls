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

(defmacro with-coveralls ((&key (project-dir (project-dir)) dry-run) &body body)
  (let ((report-file (gensym "REPORT-FILE"))
        (source-path (gensym "SOURCE-PATH"))
        (normalized-source-path (gensym "NORMALIZED-SOURCE-PATH"))
        (root-dir (gensym "ROOT-DIR")))
    `(when (asdf::getenv "COVERALLS")
       (let ((,root-dir (and ,project-dir
                             (namestring (probe-file ,project-dir)))))
         (initialize-coverage)
         (prog1 (unwind-protect (progn ,@body)
                  (disable-coverage))
           (report-to-coveralls
            (loop for ,report-file in (finalize-coverage)
                  for ,source-path = (source-path-of-report-file ,report-file)
                  for ,normalized-source-path = (cond
                                                  ((null ,root-dir)
                                                   ,source-path)
                                                  ((string= ,root-dir
                                                            ,source-path
                                                            :end2 (length ,root-dir))
                                                   (subseq ,source-path (length ,root-dir)))
                                                  (t nil))
                  when ,normalized-source-path collect
                    `(("name" . ,,normalized-source-path)
                      ("source_digest" . ,(ironclad:byte-array-to-hex-string
                                           (ironclad:digest-file :md5 ,source-path)))
                      ("coverage" . ,(get-coverage-from-report-file ,report-file))))
            :dry-run ,dry-run))))))

(defun service-name ()
  (cond
    ((asdf::getenv "TRAVIS") :travis-ci)
    (t :travis-ci)))

(defun service-job-id (&optional (service-name (service-name)))
  (ecase service-name
    (:travis-ci (asdf::getenv "TRAVIS_JOB_ID"))))

(defun project-dir (&optional (service-name (service-name)))
  (ecase service-name
    (:travis-ci (asdf::getenv "TRAVIS_BUILD_DIR"))))
