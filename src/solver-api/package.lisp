;;;;
;;;; Solver API package
;;;;
(defpackage #:com.kjcjohnson.ks2.solver-api
  (:use #:cl)
  ;; Registration API
  (:export #:register-solver
           #:list-solvers)
  ;; Metadata API
  (:export #:solver-name
           #:solver-symbols
           #:solver-description
           #:solver-action
           #:solver-options
           #:solver-option
           #:make-solver-option
           #:define-solver-metadata)
  ;; Solving API
  (:export #:transform-specification
           #:smt-solver-configuration
           #:initialize-solver
           #:solve-problem))
