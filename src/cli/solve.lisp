;;;;
;;;; Implementation of the solve command
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

(defun result-to-exit-code (result)
  (case (sv:status result)
    (:solved 0)
    (:timeout 3)
    (:crash 4)
    (:unsupported 5)
    (:unrealizable 6)
    (:error 7)
    (:unknown 8)
    (otherwise 9)))

(defun invoke-solve (solver core problems &key timeout)
  "Invokes the solve command with SOLVER in CORE and PROBLEMS"
  (let ((statuses nil))
    (dolist (pr-path problems)
      (format t "~&~a:~%" pr-path)
      (let* ((problem (sv:make-problem
                       (u:rationalize-namestring pr-path)
                       :transformer #'ks2:ensure-sexp-benchmark-file))
             (result (sv:run-problem problem solver core :timeout timeout)))
        (fresh-line)
        (sv:print-result result)
        (push (result-to-exit-code result) statuses)))
    (nreverse statuses)))
