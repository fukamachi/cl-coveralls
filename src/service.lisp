(in-package :cl-user)
(defpackage cl-coveralls.service
  (:use :cl)
  (:import-from #:alexandria
                #:when-let
                #:when-let*)
  (:export #:service-name
           #:service-job-id
           #:project-dir
           #:commit-sha
           #:pull-request-num))
(in-package :cl-coveralls.service)

(defun service-name ()
  (cond
    ((uiop:getenv "TRAVIS") :travis-ci)
    ((uiop:getenv "CIRCLECI") :circleci)
    ((uiop:getenv "GITHUB_ACTIONS") :github)
    (t :manual)))

(defun service-job-id (&optional (service-name (service-name)))
  (ecase service-name
    (:travis-ci (uiop:getenv "TRAVIS_JOB_ID"))
    (:circleci (uiop:getenv "CIRCLE_BUILD_NUM"))
    (:github (uiop:getenv "GITHUB_RUN_ID"))
    (:manual (format nil "~A"
                     (get-universal-time)))))

(defun project-dir (&optional service-name)
  (case service-name
    (:travis-ci
     (when-let (build-dir (uiop:getenv "TRAVIS_BUILD_DIR"))
       (uiop:ensure-directory-pathname build-dir)))
    (:circleci
     (when-let (build-dir (uiop:getenv "CIRCLE_PROJECT_REPONAME"))
       (uiop:ensure-directory-pathname
        (merge-pathnames build-dir (user-homedir-pathname)))))
    (:github
     (when-let (build-dir (uiop:getenv "GITHUB_WORKSPACE"))
       (uiop:ensure-directory-pathname build-dir)))
    (otherwise *default-pathname-defaults*)))

(defun commit-sha (&optional (service-name (service-name)))
  (ecase service-name
    (:travis-ci (uiop:getenv "TRAVIS_COMMIT"))
    (:circleci (uiop:getenv "CIRCLE_SHA1"))
    (:github (uiop:getenv "GITHUB_SHA"))
    (:manual (uiop:symbol-call :cl-coveralls.git :git-sha))))

(defun pull-request-num (&optional (service-name (service-name)))
  (ecase service-name
    (:travis-ci (uiop:getenv "TRAVIS_PULL_REQUEST"))
    (:circleci (uiop:getenv "CIRCLE_PR_NUMBER"))
    (:github (when-let* ((event-name (uiop:getenv "GITHUB_EVENT_NAME"))
                         (event-file (uiop:getenv "GITHUB_EVENT_PATH"))
                         (file-content (uiop:read-file-string event-file))
                         (event (let ((json:*json-identifier-name-to-lisp* #'identity)
                                      (json:*identifier-name-to-key* #'identity))
                                  (json:decode-json-from-string file-content))))
               ;; This processing taken from official Coveralls GH Action:
               ;; https://github.com/coverallsapp/github-action/blob/8cbef1dea373ebce56de0a14c68d6267baa10b44/src/run.ts#L38-L40
               (cdr (assoc "number" event :test 'equal))))
    (:manual nil)))
