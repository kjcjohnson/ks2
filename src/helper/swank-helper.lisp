;;;;
;;;; Swank helper for the ks2 runner
;;;;
(eval-when (:compile-toplevel :load-toplevel)
  (when (find-package '#:com.kjcjohnson.ks2.runner.helper)
    (delete-package '#:com.kjcjohnson.ks2.runner.helper)))

(defpackage #:com.kjcjohnson.ks2.runner.helper
  (:use :cl)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:spec #:com.kjcjohnson.synthkit.specification)
                    (#:smt #:com.kjcjohnson.synthkit.smt)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:solver-api #:com.kjcjohnson.ks2.solver-api)
                    (#:u #:com.kjcjohnson.ks2.utilities)
                    (#:sku #:com.kjcjohnson.synthkit.utilities)
                    (#:sgv #:com.kjcjohnson.synthkit.semgus.verifiers))
  (:export #:init-and-start-swank)
  (:import-from #:swank
                #:defslimefun))

(in-package #:com.kjcjohnson.ks2.runner.helper)

(defun init-and-start-swank (port-file)
  "Initialize and start a Swank server. Write the port to portfile."
  (when (uiop:file-exists-p port-file)
    (delete-file port-file))

  (setf swank:*configure-emacs-indentation* nil)

  (swank:start-server port-file :dont-close t)

  (loop (sleep 60)))

(defslimefun get-used-dynamic-space ()
  (sb-kernel:dynamic-usage))

(defslimefun get-total-dynamic-space ()
  (sb-ext:dynamic-space-size))

(defslimefun get-execution-counter ()
  ast:*execution-counter*)

(defslimefun get-concrete-candidate-counter ()
  ast:*candidate-concrete-programs*)

(defslimefun get-partial-candidate-counter ()
  ast:*candidate-partial-programs*)

(defslimefun get-concrete-candidates-by-size ()
  ast:*concrete-candidates-by-size*)

(defslimefun get-checkpoint-times ()
  ast:*checkpoint-times*)

(defslimefun force-gc ()
  (trivial-garbage:gc :full t))

(defslimefun get-gc-run-time ()
  #+sbcl sb-ext:*gc-run-time*
  #-sbcl nil)

(defslimefun set-core-options (&key program-compile debug-compile force-semgus-verifier
                                    &allow-other-keys)
  (setf ast:*use-program-compiler* program-compile)
  (setf semgus:*debug-compile* debug-compile)
  (setf semgus:*force-semgus-verifier* force-semgus-verifier))

(defslimefun get-statistics ()
  (list
   :dynamic-space-used (sb-kernel:dynamic-usage)
   :dynamic-space-total (sb-ext:dynamic-space-size)
   :gc-run-time sb-ext:*gc-run-time*
   :execution-counter ast:*execution-counter*
   :internal-real-time (get-internal-real-time)
   :internal-run-time (get-internal-run-time)
   :load-semgus-problem-time semgus:*load-semgus-problem-time*))

(defslimefun get-load-time ()
  semgus:*load-semgus-problem-time*)

(defslimefun get-check-program-time ()
  (sku:get-timed-section-real-time semgus:*check-program-time*))

(defslimefun get-full-verifier-stats ()
  (list
   :full-verifier-time (sku:get-timed-section-real-time sgv::*full-check-section*)
   :full-verifier-count sgv::*full-check-count*
   :quick-verifier-count sgv::*quick-check-count*))

(defslimefun bootstrap-tdp ()
  (unless (find :ks2-bootstrapped *features*)
    (proclaim '(optimize (speed 3) (debug 0)))
    (asdf:oos 'asdf:load-op "com.kjcjohnson.tdp/test" :force t)
    (push :ks2-bootstrapped *features*))
  (setf ast:*execution-counter* 0)
  (setf ast:*candidate-concrete-programs* 0)
  (setf ast:*candidate-partial-programs* 0)
  (setf ast:*prune-candidate-counter* 0)
  (setf ast:*prune-attempt-counter* 0)
  (setf ast:*prune-success-counter* 0)
  (ast:clear-all-checkpoints)
  (sku:reset-timed-section-time semgus:*check-program-time*)
  (trivial-garbage:gc :full t)
  (when (uiop:getenv "KS2_SMT_DEBUG")
    (setf cl-smt-lib:*smt-debug* t))
  t)

(defvar *problem-file* nil "The current loaded problem file")

(defun maybe-load-problem-file (problem-file)
  "Loads a problem file if specified, or falls back to *PROBLEM-FILE*"
  (if problem-file
      (semgus:load-semgus-problem problem-file)
      *problem-file*))

(defslimefun load-problem-file (problem-file)
  (setf *problem-file* (semgus:load-semgus-problem problem-file))
  t)

;;;
;;; Solver API RPCs
;;;
(defslimefun list-solvers ()
  (solver-api:list-solvers))

(defslimefun solver-name (solver-designator)
  (solver-api:solver-name (solver-api:resolve-solver solver-designator)))

(defslimefun solver-symbols (solver-designator)
  (solver-api:solver-symbols (solver-api:resolve-solver solver-designator)))

(defslimefun solver-description (solver-designator)
  (solver-api:solver-description (solver-api:resolve-solver solver-designator)))

(defslimefun solver-action (solver-designator)
  (solver-api:solver-action (solver-api:resolve-solver solver-designator)))

(defslimefun solver-options (solver-designator)
  (solver-api:solver-options (solver-api:resolve-solver solver-designator)))

(defun actually-print-program-node (pn)
  "Prints a decent representation of a program node PN."
  (if (typep pn 'ast:program-atom)
      (let ((ast::*printing-program-node* t))
        (with-output-to-string (string-stream)
          (ast::print-program-node pn string-stream)))
      (format nil "~s" pn)))

(defun transform-problem (solver problem)
  "Transforms PROBLEM into a format usable by SOLVER."
  (semgus:replace-specification
   problem
   (solver-api:transform-specification solver
                                       (semgus:specification problem)
                                       (semgus:context problem))))

(defmethod solver-api:smt-solver-configuration (solver &key &allow-other-keys)
  "Default cvc5 SMT solver"
  (make-instance 'smt:solver*
                 :program (u:locate-file "cvc5"
                                         :optional-suffix "exe")
                 :arguments (smt:arguments smt:*cvc5*)))

(defmethod solver-api:smt-solver-configuration :around (solver &key &allow-other-keys)
  "Looks for a solver at the right location"
  (let ((config (call-next-method)))
    (if (pathnamep (smt:program config))
        config
        (make-instance 'smt:solver*
                       :program (u:locate-file (smt:program config)
                                               :optional-suffix "exe")
                       :arguments (smt:arguments config)))))

(defun do-solve-problem (solver problem options)
  "Solves a problem."
  (let ((solver-spec (solver-api:smt-solver-configuration solver)))
    (smt:with-lazy-solver (solver-spec)
      (apply #'solver-api:solve-problem solver problem options))))

(defun %get-spec-types (problem)
  "Gets the specification types used in PROBLEM"
  (map 'list #'symbol-name
       (and problem
            (semgus:specification problem)
            (spec:leaf-specification-types (semgus:specification problem)))))

(defun get-smt-results (results problem)
  "Creates an SMT string of RESULTS for PROBLEM"
  (with-output-to-string (s)
    (format s "(~%")
    ;; This is a little scuff, because we don't have mappings between synthfuns
    ;; and the associated results. So we just assume they're all the same _shrug_
    (flet ((print-result (r)
             (format s "  (define-fun ~a () ~a "
                     (smt:identifier-string
                      (semgus:term-name (semgus:context problem)))
                     (let ((sort (smt:identifier-string
                                  (smt:name (semgus:term-type
                                             (semgus:context problem))))))
                       (if (listp sort) ; Ugh.
                           (if (= 1 (length sort))
                               (first sort)
                               (format nil "(_ ~{~a~^ ~})" sort))
                           sort)))
             (ast:print-program-node-as-smt r s)
             (format s ")~%")))
      (if (atom results)
          (print-result results)
          (map nil #'print-result results)))
    (format s ")")))

(defun create-result (results delta-internal-time problem)
  "Creates a results object to pass back"
  ;; TODO: create and serialize results into a proxy object
  (list
   :program (cond
              ((and (listp results) (= 1 (length results)))
               (actually-print-program-node (first results)))
              ((typep results 'sequence)
               (format nil "~s"
                       (map 'list #'actually-print-program-node results)))
              (t
               (actually-print-program-node results)))
   :program-as-smt (get-smt-results results problem)
   :time (/ delta-internal-time internal-time-units-per-second)
   :spec-types (%get-spec-types problem)
   :checkpoint-times ast:*checkpoint-times*
   :prune-candidate-counter ast:*prune-candidate-counter*
   :prune-attempt-counter ast:*prune-attempt-counter*
   :prune-success-counter ast:*prune-success-counter*
   :gc-run-time #+sbcl sb-ext:*gc-run-time* #-sbcl nil
   :load-semgus-problem-time semgus:*load-semgus-problem-time*
   :check-program-time (sku:get-timed-section-real-time semgus:*check-program-time*)
   :full-verifier-time (sku:get-timed-section-real-time sgv::*full-check-section*)
   :full-verifier-count sgv::*full-check-count*
   :quick-verifier-count sgv::*quick-check-count*))


(defslimefun solve-problem
    (solver-designator problem-file &rest options &key &allow-other-keys)
  (let ((solver (solver-api:resolve-solver solver-designator)))
    (apply #'solver-api:initialize-solver solver options)
    (let* ((problem (maybe-load-problem-file problem-file))
           (new-p (transform-problem solver problem)))

      (if (null (semgus:specification new-p))
          (list :program :unsupported :spec-types (%get-spec-types problem))
          (let* ((start-time (get-internal-real-time))
                 (results (do-solve-problem solver new-p options))
                 (end-time (get-internal-real-time)))
            (create-result results (- end-time start-time) new-p))))))
