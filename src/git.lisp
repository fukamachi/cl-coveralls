(in-package :cl-user)
(defpackage cl-coveralls.git
  (:use :cl)
  (:import-from :cl-coveralls.service
                :project-dir)
  (:import-from :alexandria
                :ends-with-subseq)
  (:export :git-branch
           :git-sha
           :author-name
           :author-email
           :committer-name
           :committer-email
           :commit-message))
(in-package :cl-coveralls.git)

(defun git (command &key (project-dir (project-dir)))
  (check-type command list)
  (let ((result
          (with-output-to-string (s)
            (uiop:run-program `("git" "--git-dir" ,(namestring
                                                    (merge-pathnames #P".git"
                                                                     (pathname project-dir))) ,@command)
                              :output s))))
    (if (ends-with-subseq #.(string #\Newline) result)
        (subseq result 0 (1- (length result)))
        result)))

(defun git-branch ()
  (or (uiop:getenv "GIT_BRANCH")
      (uiop:getenv "GITHUB_REF")
      (git '("rev-parse" "--abbrev-ref" "HEAD"))))

(defun git-sha ()
  (git '("rev-parse" "HEAD")))

(defun author-name ()
  (git '("log" "-1" "--pretty=%aN")))

(defun author-email ()
  (git '("log" "-1" "--pretty=%aE")))

(defun committer-name ()
  (git '("log" "-1" "--pretty=%cN")))

(defun committer-email ()
  (git '("log" "-1" "--pretty=%cE")))

(defun commit-message ()
  (git '("log" "-1" "--pretty=%s")))
