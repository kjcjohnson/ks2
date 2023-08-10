;;;;
;;;; Implementation of the solve command
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

(defun invoke-solve (solver problems &key timeout)
  "Invokes the solve command with SOLVER and PROBLEMS"
  (dolist (pr-path problems)
    (format t "~&~a:~%" pr-path)
    (let* ((problem (sv:make-problem
                     (u:rationalize-namestring pr-path)
                     :transformer #'ks2:ensure-sexp-benchmark-file))
           (result (sv:run-problem problem solver :timeout timeout)))
      (fresh-line)
      (sv:print-result result))))
