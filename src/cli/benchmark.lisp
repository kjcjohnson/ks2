;;;;
;;;; Implementation of the benchmark command
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

(defgeneric write-result (format result output-spec)
  (:documentation "Writes RESULT with format FORMAT. OUTPUT-SPEC is either a
directory pathname (to write to file), t to write to stdout, or nil for a string."))

(defun invoke-benchmark (suite-dir solvers &key output-format output-path)
  "Invokes a benchmark suite"
  (let ((suite-pathname (u:rationalize-namestring suite-dir :directory t))
        (output-pathname (u:rationalize-namestring output-path :directory t)))
    (unless (uiop:directory-exists-p suite-pathname)
      (error "suite directory ~a does not exist" suite-pathname))
    
    (let* ((suite (sv:make-suite-from-directory suite-pathname))
           (results (sv:run-suite suite solvers)))
      (write-result output-format results output-pathname))))


;;;
;;; JSON output
;;;
(defmethod write-result ((format (eql :json)) (result sv:suite-results) output-spec)
  "Writes suite results to JSON"
  (cond
    ((uiop:directory-pathname-p output-spec)
     (ensure-directories-exist output-spec)
     (uiop:with-output-file (fs (uiop:merge-pathnames*
                                 (make-pathname :name (sv:name (sv:suite result))
                                                :type "json")
                                 output-spec)
                                :if-exists :supersede)
       (shasht:write-json result fs)))
    ((null output-spec)
     (shasht:write-json result nil))
    ((eql t output-spec)
     (shasht:write-json result t))
    (t
     (error "Invalid output-spec: ~a" output-spec))))
                                                                       
