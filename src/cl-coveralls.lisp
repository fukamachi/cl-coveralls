(in-package :cl-user)
(defpackage cl-coveralls
  (:nicknames :coveralls)
  (:use :cl)
  (:import-from :cl-coveralls.service
                :service-name
                :service-job-id
                :project-dir
                :commit-sha
                :pull-request-num)
  (:import-from :cl-coveralls.git
                :git-branch
                :author-name
                :author-email
                :committer-name
                :committer-email
                :commit-message)
  (:import-from :cl-coveralls.impls
                :enable-coverage
                :disable-coverage
                :initialize-coverage
                :finalize-coverage
                :source-path-of-report-file
                :get-coverage-from-report-file)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :digest-file)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from :alexandria
                :when-let
                :with-gensyms
                :ensure-list)
  (:export :with-coveralls
           :calc-system
           :calc-coverage))
(in-package :cl-coveralls)


(defun mask-secret (text)
  (let* ((len (length text))
         (max-prefix-len 4)
         (prefix-len (min (floor (/ len 4))
                          max-prefix-len)))
    (with-output-to-string (s)
      (write-string (subseq text 0 prefix-len)
                    s)
      (loop for i below (- len (* 2 prefix-len))
            do (write-char #\* s))
      (write-string (subseq text (- len prefix-len))
                    s))))


(defun report-to-coveralls (reports &key dry-run)
  (unless reports
    (return-from report-to-coveralls))

  (let ((repo-token (or (uiop:getenv "COVERALLS_REPO_TOKEN")
                        ""))
        (service (service-name)))
    (when (eql service
               :manual)
      (setf dry-run t))
    
    (when (and (string= repo-token "")
               (not dry-run))
      ;; https://docs.coveralls.io/api-reference says "repo_token" is required
      (error "Please, set COVERALLS_REPO_TOKEN env variable. It is required."))
    
    (let* ((json-data
             (append
              (list (cons "service_name" (string-downcase service))
                    (cons "service_job_id" (service-job-id))
                    (cons "repo_token" repo-token))
              (when-let (pullreq (pull-request-num))
                (list (cons "service_pull_request" pullreq)))
              (list
               (cons "git" (list
                            (cons "head"
                                  (list (cons "id" (commit-sha))
                                        (cons "author_name" (author-name))
                                        (cons "author_email" (author-email))
                                        (cons "committer_name" (committer-name))
                                        (cons "committer_email" (committer-email))
                                        (cons "message" (commit-message))))
                            (cons "branch" (git-branch))))
               (cons "source_files" (coerce reports 'simple-vector)))))
           (json (jojo:to-json json-data :from :alist))
           (secure-json (progn
                          ;; Mask the secret repo token
                          (when repo-token
                            (rplacd (assoc "repo_token" (cdr json-data) :test #'string=)
                                    (mask-secret repo-token)))
                          (jojo:to-json json-data :from :alist))))

      (cond
        (dry-run
         ;; Here we convert data again
         (format t "~A~%"
                 secure-json))
        (t
         (let ((json-file (uiop:with-temporary-file (:stream out :direction :output :keep t)
                            (write-string json out)
                            (pathname out)))
               (retry-handler (dex:retry-request 5 :interval 3)))
           (format t "~&Sending coverage report to Coveralls...~2%~A~%" secure-json)
           (handler-bind ((dex:http-request-failed
                            (lambda (c)
                              (let ((headers (alexandria:hash-table-alist (dex:response-headers c))))
                                (format t "Server respond with: ~A~2%Headers:~%~S~2%Body:~%~A~2%Retrying~%"
                                        (dex:response-status c)
                                        headers
                                        (dex:response-body c)))
                              (funcall retry-handler
                                       c))))
             (dex:post "https://coveralls.io/api/v1/jobs"
                       :content `(("json_file" . ,json-file))))))))))

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

(defun parse-report-files (report-files &key exclude project-dir)
  (let ((root-dir (namestring (probe-file project-dir))))
    (loop for report-file in report-files
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
            ("source_digest" . ,(ironclad:byte-array-to-hex-string
                                  (ironclad:digest-file :md5 source-path)))
            ("coverage" . ,(get-coverage-from-report-file report-file))))))

(defun get-coverage (fn &key exclude project-dir)
  (unless project-dir
    (error "Project directory is undefined"))
  (initialize-coverage)
  (loop for file in (uiop:directory-files project-dir)
        when (string= (pathname-type file) "asd")
        do (let ((system-name (pathname-name file)))
             (if (asdf:component-loaded-p system-name)
                 (handler-bind (#+sbcl (warning #'muffle-warning))
                   (asdf:load-system system-name :force t))
                 #+quicklisp (ql:quickload system-name)
               #-quicklisp (asdf:load-system system-name))))
  (let ((result (unwind-protect (funcall fn)
                  (disable-coverage)))
        (report-files (finalize-coverage)))
    (values
      (parse-report-files report-files :exclude exclude :project-dir project-dir)
      result)))

(defun calc-system (system &key exclude)
  (calc-coverage (lambda () (asdf:test-system system))
                 :project-dir (asdf:system-source-directory system)
                 :exclude exclude))

(defun calc-coverage (fn &key project-dir exclude)
  (unless project-dir
    (error ":project-dir is required"))
  (loop for report in (get-coverage fn :exclude exclude :project-dir project-dir)
        for coverage = (cdr (assoc "coverage" report :test #'string=))
        sum (count :null coverage :test-not #'eql) into all
        sum (count 1 coverage) into pass
        finally (return (/ (round (* (/ pass all) 10000)) 100.0))))

(defmacro with-coveralls ((&key exclude dry-run (project-dir '(project-dir))) &body body)
  "Sends coverage report to the Coveralls.

   If dry run specified or code started not inside one of supported CI service,
   then just prints JSON prepared for sending.
   "
  (with-gensyms (reports result)
    `(if (and (stringp (asdf::getenv "COVERALLS"))
              (string/= (asdf::getenv "COVERALLS") ""))
         (multiple-value-bind (,reports ,result)
             (get-coverage (lambda () ,@body)
                           :exclude ,exclude :project-dir ,project-dir)
           (report-to-coveralls ,reports :dry-run ,dry-run)
           ,result)
         (progn ,@body))))
