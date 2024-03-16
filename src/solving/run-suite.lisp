;;;;
;;;; Running suites
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(defun run-suite (suite solver cores &key timeout)
  "Runs a suite."
  (declare (type suite suite))
  (let ((solvers (cond ((null solver) (error "No solvers specified"))
                       ((atom solver) (list solver))
                       (t solver))))
    (assert (= (length solvers) (length cores)))
    (let ((results (make-suite-results suite (map 'list #'name solvers))))
      (loop for problem in (problems suite) do
        (format t "~&+++++ ~a +++++ [~a]~%~%" (name problem) (path problem))
        (loop for solver in solvers
              for core in cores do
          (format t "~&  --> ~a~%" solver)
          (let ((result (run-problem problem solver core :timeout timeout)))
            (setf (suite-result results (name solver) problem)
                  result))))
      results)))
