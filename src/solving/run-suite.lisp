;;;;
;;;; Running suites
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(defun run-suite (suite &optional solver)
  "Runs a suite."
  (declare (type suite suite))
  (let ((solvers (cond ((null solver) runner::*solvers*)
                       ((atom solver) (list solver))
                       (t solver))))
    (let ((results (make-suite-results suite solvers)))
      (loop for problem in (problems suite) do
        (format t "~&+++++ ~a +++++ [~a]~%~%" (name problem) (path problem))
        (loop for solver in solvers do
          (format t "~&  --> ~a~%" solver)
          (let ((result (run-problem problem solver)))
            (setf (suite-result results solver problem)
                  result))))
      results)))
    
