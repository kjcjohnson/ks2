;;;;
;;;; API functions for communicating with the child instance
;;;;
(in-package #:com.kjcjohnson.ks2.runner)

(define-condition rpc-error () ())
(define-condition swank-crash () ())

(defun ok-or-fail (thing)
  "Gets the return value of a rex response"
  (case (first thing)
    (:ok (values-list (map 'list #'ms:unmarshal (rest thing))))
    (:abort (error (make-condition 'rpc-error)))
    (:error (error (make-condition 'rpc-error)))
    (:crash (error (make-condition 'swank-crash)))
    (otherwise (error "Unknown REX response: ~s" thing))))

(defun get-total-dynamic-space (child-lisp)
  "Gets the total dynamic space in a child lisp environment."
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-total-dynamic-space))))

(defun get-used-dynamic-space (child-lisp)
  "Gets the used dynamic space in a child lisp environment."
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-used-dynamic-space))))

(defun get-execution-counter (child-lisp)
  "Gets the execution counter in a child lisp environment."
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-execution-counter))))

(defun get-concrete-candidate-counter (child-lisp)
  "Gets the candidate concrete program counter in a child lisp environment."
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-concrete-candidate-counter))))

(defun get-partial-candidate-counter (child-lisp)
  "Gets the candidate partial program counter in a child lisp environment."
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-partial-candidate-counter))))

(defun get-concrete-candidates-by-size (child-lisp)
  "Gets the concrete candidates size mapping in a child lisp environment."
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-concrete-candidates-by-size))))

(defun get-checkpoint-times (child-lisp)
  "Gets the times associated with each checkpoint in a child lisp environment"
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-checkpoint-times))))

(defun get-statistics (child-lisp)
  "Gets statistics from a child lisp environment"
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-statistics))))

(defun bootstrap-tdp (child-lisp)
  "Bootstraps the TDP stuff in the child environment."
  (ok-or-fail (rpc-call/sync child-lisp '(helper:bootstrap-tdp))))

(defun complete-fulfill-function (promise)
  "Creates a function to parse an RPC response and fulfill the given promise."
  #'(lambda (result)
      (lparallel:fulfill promise (ok-or-fail result))))

(defun force-gc (child-lisp)
  (ok-or-fail (rpc-call/sync child-lisp '(helper:force-gc))))

(defun get-gc-run-time (child-lisp)
  (ok-or-fail (rpc-call/sync child-lisp '(helper:get-gc-run-time))))

(defun load-problem-file (child-lisp problem-file)
  (ok-or-fail (rpc-call/sync child-lisp `(helper:load-problem-file ,problem-file))))

;;;
;;; Solver API
;;;
(defun list-solvers (child-lisp)
  (ok-or-fail
   (rpc-call/sync child-lisp '(helper:list-solvers))))

(defun solver-name (child-lisp solver-designator)
  (ok-or-fail
   (rpc-call/sync child-lisp `(helper:solver-name ,solver-designator))))

(defun solver-symbols (child-lisp solver-designator)
  (ok-or-fail
   (rpc-call/sync child-lisp `(helper:solver-symbols ,solver-designator))))

(defun solver-description (child-lisp solver-designator)
  (ok-or-fail
   (rpc-call/sync child-lisp `(helper:solver-description ,solver-designator))))

(defun solver-action (child-lisp solver-designator)
  (ok-or-fail
   (rpc-call/sync child-lisp `(helper:solver-action ,solver-designator))))

(defun solver-options (child-lisp solver-designator)
  (ok-or-fail
   (rpc-call/sync child-lisp `(helper:solver-options ,solver-designator))))

(defun solve-problem (child-lisp solver-designator problem-file
                      &rest options &key &allow-other-keys)
  (ok-or-fail
   (rpc-call/sync
    child-lisp
    `(helper:solve-problem ,solver-designator ,problem-file ,@options))))

(defun solve-problem/promise (child-lisp solver-designator problem-file
                              &rest options &key &allow-other-keys)
  (rpc-call/promise
   child-lisp
   `(helper:solve-problem ,solver-designator ,problem-file ,@options)))
