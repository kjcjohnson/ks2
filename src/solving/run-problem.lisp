;;;;
;;;; Running a problem
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(defvar *core-output-stream* (make-synonym-stream '*standard-output*)
  "Stream to write core output to")

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
                  (runner:get-checkpoint-times child-lisp))
    (set-not-null :load-time
                  (runner::get-load-time child-lisp))
    (set-not-null :gc-run-time
                  (runner::get-gc-run-time child-lisp))
    (set-not-null :check-program-time
                  (runner::get-check-program-time child-lisp))
    (let ((fvs (runner::get-full-verifier-stats child-lisp)))
      (set-not-null :full-verifier-time (getf fvs :full-verifier-time))
      (set-not-null :full-verifier-count (getf fvs :full-verifier-count))
      (set-not-null :quick-verifier-count (getf fvs :quick-verifier-count)))))

(defun %run-problem (problem solver core-cfg &key timeout)
  "Runs a benchmark on a solver."
  (let (child-lisp core)
    (unwind-protect
         (progn
           (setf core (core:start-core core-cfg :output *core-output-stream*))
           (setf child-lisp (systems.duck.ks2.core.synthkit::child-lisp core))
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
                   doing (sleep 1))

             (let* ((solved? (lparallel:fulfilledp result-promise))
                    (time (runner::internal-time-to-seconds (- (get-internal-real-time)
                                                       start-time)))
                    (memory (- (runner::bytes-to-mebibytes max-memory)
                               (runner::bytes-to-mebibytes initial-memory)))
                    (result (if solved?
                                (runner::ok-or-fail (lparallel:force result-promise))
                                nil))
                    (exec-count (runner:get-execution-counter child-lisp))
                    (concrete-count (runner:get-concrete-candidate-counter child-lisp))
                    (partial-count (runner:get-partial-candidate-counter child-lisp))
                    (cand-by-size (runner:get-concrete-candidates-by-size child-lisp))
                    (checkpoint-times (or (getf result :checkpoint-times)
                                          (runner:get-checkpoint-times child-lisp)))
                    (program (getf result :program))
                    (program-as-smt (getf result :program-as-smt))
                    (real-time (getf result :time))
                    (load-time (or (getf result :load-semgus-problem-time)
                                   (getf *live-data-stash* :load-time)))
                    (gc-run-time (or (getf result :gc-run-time)
                                     (getf *live-data-stash* :gc-run-time)))
                    (check-program-time (or
                                         (getf result :check-program-time)
                                         (getf *live-data-stash* :check-program-time)))
                    ;; TIME starts from when we call the solver, so we need to
                    ;;  subtract off the load time. REAL-TIME starts ticking
                    ;;  after the problem is loaded.
                    (exec-rate (/ exec-count (or real-time
                                                 (- time (or load-time 0)))))
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
                                    load-time
                                    (/ gc-run-time internal-time-units-per-second)
                                    check-program-time
                                    memory
                                    (if (and (listp program) (= 1 (length program)))
                                        (first program)
                                        program)
                                    program-as-smt
                                    exec-rate
                                    spec-types
                                    concrete-count
                                    partial-count
                                    cand-by-size
                                    checkpoint-times
                                    prune-candidates
                                    prune-attempts
                                    prune-successes
                                    prune-ratio
                                    (or (getf result :full-verifier-time)
                                        (getf *live-data-stash* :full-verifier-time))
                                    (or (getf result :full-verifier-count)
                                        (getf *live-data-stash* :full-verifier-count))
                                    (or (getf result :quick-verifier-count)
                                        (getf *live-data-stash* :quick-verifier-count))))))
      (unless (null child-lisp)
        (core:stop-core core :urgent t)))))

(defun run-problem (problem solver core &key timeout)
  "Runs a synthesis problem"
  (declare (type problem problem)
           (type solver-config solver)
           (type core:core-config core))
  (let* ((*live-data-stash* nil)
         (result
           (block do-solve
             (handler-bind
                 ((runner::rpc-error
                    #'(lambda (e)
                        (declare (ignore e))
                        (return-from do-solve
                          (make-problem-result-for-error (name problem)
                                              (name solver)
                                              :live *live-data-stash*))))
                  (runner::swank-crash
                    #'(lambda (e)
                        (declare (ignore e))
                        (return-from do-solve
                          (make-problem-result-for-crash (name problem)
                                              (name solver)
                                              :live *live-data-stash*))))
                  (solver-config-error
                    #'(lambda (e)
                        (force-output)
                        (format *error-output* "error: ~a" e)
                        (error "error: fatal configuration error")))
                  (error
                    #'(lambda (e)
                        (v:error :solving
                                 "Caught unknown condition while running a problem")
                        (v:error :solving e)
                        (return-from do-solve
                          (make-problem-result-for-unknown (name problem)
                                                           (name solver)
                                                           :live *live-data-stash*)))))

               (%run-problem problem solver core :timeout timeout)))))
    result))
