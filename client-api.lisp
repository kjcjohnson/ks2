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

(defun bootstrap-tdp (child-lisp)
  "Bootstraps the TDP stuff in the child environment."
  (ok-or-fail (rpc-call/sync child-lisp '(helper:bootstrap-tdp))))

(defun complete-fulfill-function (promise)
  "Creates a function to parse an RPC response and fulfill the given promise."
  #'(lambda (result)
      (lparallel:fulfill promise (ok-or-fail result))))

(defun enum-solve/promise (child-lisp problem-file
                           &rest options
                           &key max-depth &allow-other-keys)
  (declare (ignore max-depth))
  (rpc-call/promise child-lisp
                    `(helper:enum-solve ,problem-file ,@options)))

(defun enum-solve (child-lisp problem-file &key max-depth)
  (ok-or-fail (rpc-call/sync child-lisp
                             `(helper:enum-solve ,problem-file
                                                 :max-depth ,max-depth))))

(defun duet-solve/promise (child-lisp problem-file
                           &rest options
                           &key &allow-other-keys)
  (rpc-call/promise child-lisp
                    `(helper:duet-solve ,problem-file ,@options)))

(defun duet-solve (child-lisp problem-file &key depth)
  (ok-or-fail (rpc-call/sync child-lisp
                             `(helper:duet-solve ,problem-file :depth ,depth))))

(defun frangel-solve/promise (child-lisp problem-file
                              &rest options
                              &key &allow-other-keys)
  (rpc-call/promise child-lisp
                    `(helper:frangel-solve ,problem-file ,@options)))

(defun frangel-solve (child-lisp problem-file)
  (ok-or-fail (rpc-call/sync child-lisp
                             `(helper:frangel-solve ,problem-file))))
(defun tde-solve/promise (child-lisp problem-file
                           &rest options
                           &key &allow-other-keys)
  (rpc-call/promise child-lisp
                    `(helper:tde-solve ,problem-file ,@options)))

(defun tde-solve (child-lisp problem-file)
  (ok-or-fail (rpc-call/sync child-lisp
                             `(helper:tde-solve ,problem-file))))

(defun force-gc (child-lisp)
  (ok-or-fail (rpc-call/sync child-lisp (helper:force-gc))))
