;;;;
;;;; Running a problem
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(defvar *live-data-stash* nil "A place for live data to be stashed, so that we can
report on some statistics when a solving run crashes.")

(defun initialize-live-data ()
  "Sets up the live data stash"
  (setf (getf *live-data-stash* :start-time)
        (get-internal-real-time)))

(defun populate-live-data (child-lisp)
  "Adds the most recent stats to the live data stash from CHILD-LISP"
  (flet ((set-not-null (key value)
           "Sets the live data KEY, but only if VALUE isn't null"
           (unless (null value)
             (setf (getf *live-data-stash* key) value))))
    (set-not-null :max-memory
                  (max (or (getf *live-data-stash* :max-memory) 0)
                       (runner::get-used-dynamic-space child-lisp)))
    (set-not-null :execution-counter
                  (runner::get-execution-counter child-lisp))
    (set-not-null :concrete-candidate-counter
                  (runner:get-concrete-candidate-counter child-lisp))
    (set-not-null :partial-candidate-counter
                  (runner:get-partial-candidate-counter child-lisp))
    (set-not-null :concrete-candidates-by-size
                  (runner:get-concrete-candidates-by-size child-lisp))
    (set-not-null :checkpoint-times
                  (runner:get-checkpoint-times child-lisp))))

(defun %run-problem (problem solver &key timeout)
  "Runs a benchmark on a solver."
  (let (child-lisp)
    (unwind-protect
         (progn
           (setf child-lisp (runner::swank-spawn))
           (runner::bootstrap-tdp child-lisp)
           (validate-solver-options child-lisp solver)
           (runner::force-gc child-lisp)
           (let* ((initial-memory (runner::get-used-dynamic-space child-lisp))
                  (result-promise (runner::start-solver child-lisp
                                                        (path problem)
                                                        (solver solver)
                                                        (option-plist solver)))
                  (start-time (get-internal-real-time))
                  (kill-time (+ (* (or timeout 120)
                                   internal-time-units-per-second)
                                start-time))
                  (max-memory 0))
             (initialize-live-data)
             (loop until (lparallel:fulfilledp result-promise)
                   doing (let ((dm (runner::get-used-dynamic-space child-lisp)))
                           (when (< max-memory dm)
                             (setf max-memory dm)))
                   doing (populate-live-data child-lisp)
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
                    (cand-by-size (runner:get-concrete-candidates-by-size child-lisp))
                    (checkpoint-times (or (getf result :checkpoint-times)
                                          (runner:get-checkpoint-times child-lisp)))
                    (program (getf result :program))
                    (real-time (getf result :time))
                    (spec-types (getf result :spec-types))
                    (prune-candidates (getf result :prune-candidate-counter))
                    (prune-attempts (getf result :prune-attempt-counter))
                    (prune-successes (getf result :prune-success-counter))
                    (prune-ratio (if (or
                                      (null prune-attempts)
                                      (null prune-successes)
                                      (zerop prune-attempts))
                                     nil
                                     (* 100 (/ prune-successes prune-attempts)))))

               (make-problem-result (name problem)
                                    (name solver)
                                    solved?
                                    (or real-time time)
                                    memory
                                    (if (and (listp program) (= 1 (length program)))
                                        (first program)
                                        program)
                                    exec-rate
                                    spec-types
                                    concrete-count
                                    partial-count
                                    cand-by-size
                                    checkpoint-times
                                    prune-candidates
                                    prune-attempts
                                    prune-successes
                                    prune-ratio))))
      (unless (null child-lisp)
        (runner::terminate-child child-lisp :urgent t)))))

(defun run-problem (problem solver &key timeout)
  "Runs a synthesis problem"
  (declare (type problem problem))
  (let* ((*live-data-stash* nil)
         (result
           (handler-case
               (%run-problem problem solver :timeout timeout)
             (runner::rpc-error ()
               (make-problem-result-for-error (name problem)
                                              (name solver)
                                              :live *live-data-stash*))
             (runner::swank-crash ()
               (make-problem-result-for-crash (name problem)
                                              (name solver)
                                              :live *live-data-stash*))
             (error (e)
               (format *error-output* "; **OTHER CONDITION: ~a~%" e)
               (make-problem-result-for-unknown (name problem)
                                                (name solver)
                                                :live *live-data-stash*)))))
    result))
