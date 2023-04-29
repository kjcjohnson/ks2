;;;;
;;;; Running a problem
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(defun %run-problem (problem solver)
  "Runs a benchmark on a solver."
  (let (child-lisp)
    (unwind-protect
         (progn
           (setf child-lisp (runner::swank-spawn))
           (runner::bootstrap-tdp child-lisp)
           (runner::force-gc child-lisp)
           (let* ((initial-memory (runner::get-used-dynamic-space child-lisp))
                  (result-promise (runner::start-solver child-lisp
                                                        (path problem) solver))
                  (start-time (get-internal-real-time))
                  (kill-time (+ (* 120 internal-time-units-per-second) start-time))
                  (max-memory 0))
             (loop until (lparallel:fulfilledp result-promise)
                   doing (let ((dm (runner::get-used-dynamic-space child-lisp)))
                           (when (< max-memory dm)
                             (setf max-memory dm)))
                   while (< (get-internal-real-time) kill-time)
                   doing (sleep 1)
                   doing (format *trace-output* ".")
                   doing (force-output *trace-output*))
             
             (let* ((solved? (lparallel:fulfilledp result-promise))
                    (time (runner::internal-time-to-seconds (- (get-internal-real-time)
                                                       start-time)))
                    (memory (- (runner::bytes-to-mebibytes max-memory)
                               (runner::bytes-to-mebibytes initial-memory)))
                    (result (if solved?
                                (runner::ok-or-fail (lparallel:force result-promise))
                                nil))
                    (exec-count (runner:get-execution-counter child-lisp))
                    (exec-rate (/ exec-count time))
                    (concrete-count (runner:get-concrete-candidate-counter child-lisp))
                    (partial-count (runner:get-partial-candidate-counter child-lisp))
                    (program (getf result :program))
                    (real-time (getf result :time))
                    (spec-types (getf result :spec-types)))

               (make-problem-result (name problem)
                                    solver
                                    solved?
                                    (or real-time time)
                                    memory
                                    (if (and (listp program) (= 1 (length program)))
                                        (first program)
                                        program)
                                    exec-rate
                                    spec-types
                                    concrete-count
                                    partial-count))))
      (unless (null child-lisp)
        (runner::terminate-child child-lisp :urgent t)))))

(defun run-problem (problem solver)
  "Runs a synthesis problem"
  (declare (type problem problem))
  (let ((result
          (handler-case
              (%run-problem problem solver)
            (runner::rpc-error ()
              (make-problem-result-for-error (name problem) solver))
            (runner::swank-crash ()
              (make-problem-result-for-crash (name problem) solver))
            (error (e)
              (format *error-output* "; **OTHER CONDITION: ~a~%" e)
              (make-problem-result-for-unknown (name problem) solver)))))
    result))
