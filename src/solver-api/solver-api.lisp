;;;;
;;;; API for solvers that are usable by ks2
;;;;
(in-package #:com.kjcjohnson.ks2.solver-api)

;;;
;;; Solver registration
;;;
(defvar *registered-solvers* nil)

(defun register-solver (solver-designator)
  "Registers a solver indicated by SOLVER-DESIGNATOR with the system."
  (pushnew solver-designator *registered-solvers*))

(defun list-solvers ()
  "Lists all solvers registered with the system. Returns a list of solver designators."
  *registered-solvers*)

;;;
;;; Solver metadata
;;;
(defgeneric solver-name (solver-designator)
  (:documentation "Gets the friendly name associated with SOLVER-DESIGNATOR"))

(defgeneric solver-symbol (solver-designator)
  (:documentation "Gets the symbol name for this solver (e.g., on command line)"))

(defgeneric solver-description (solver-designator)
  (:documentation "Gets the description associated with SOLVER-DESIGNATOR"))

(defgeneric solver-action (solver-designator)
  (:documentation "Gets the action name for this solver"))

(defgeneric solver-options (solver-designator)
  (:documentation "Gets the list of options taken by SOLVER-DESIGNATOR. Returns a list
of SOLVER-OPTION structures."))

(defstruct solver-option
  "A solver option with the following parameters:
 - KEYWORD: argument name passed to SOLVE-PROBLEM
 - NAME: human-readable string name for showing users
 - DESCRIPTION: human-readable string description for this option
 - TYPE: the option type. Currently accepted are :NUMBER, :BOOLEAN, or :STRING
 - DEFAULT: the default option value. If unbound, option will not be passed
"
  keyword name description type default)

;; Convenience macro
(defmacro define-solver-metadata (designator
                                  &key name symbol action description options)
  `(progn
     (defmethod solver-name ((solver (eql ,designator))) ,name)
     (defmethod solver-symbol ((solver (eql ,designator))) ,symbol)
     (defmethod solver-description ((solver (eql ,designator))) ,description)
     (defmethod solver-action ((solver (eql ,designator))) ,action)
     (defmethod solver-options ((solver (eql ,designator))) ,options)))

;;;
;;; Solving API
;;;
(defgeneric solve-problem (solver-designator semgus-problem &key &allow-other-keys)
  (:documentation "Solves SEMGUS-PROBLEM with a solver designated by SOLVER-DESIGNATOR.
Solver options, as returned by SOLVER-OPTIONS, will be passed as keywords."))
