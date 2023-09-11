;;;;
;;;; API for solvers that are usable by ks2
;;;;
(in-package #:com.kjcjohnson.ks2.solver-api)

;;;
;;; Solver registration
;;;
(defvar *registered-solvers* nil)

(defun register-solver (solver-designator &optional classname)
  "Registers a solver indicated by SOLVER-DESIGNATOR with the system."
  (pushnew (cons solver-designator classname) *registered-solvers* :test #'equal))

(defun list-solvers ()
  "Lists all solvers registered with the system. Returns a list of solver designators."
  (map 'list #'first *registered-solvers*))

(defun resolve-solver (solver-designator)
  "Resolves SOLVER-DESIGNATOR into an instance, if available"
  (let ((cell (assoc solver-designator *registered-solvers*)))
    (unless (null cell)
      (let ((cdr (cdr cell)))
        (if (null cdr)
            (car cell)
            (make-instance cdr))))))

;;;
;;; Solver metadata
;;;
(defgeneric solver-name (solver-designator)
  (:documentation "Gets the friendly name associated with SOLVER-DESIGNATOR"))

(defgeneric solver-symbols (solver-designator)
  (:documentation "Gets the symbol name and aliases for this solver
(e.g., on command line). Returns a list of strings."))

(defgeneric solver-description (solver-designator)
  (:documentation "Gets the description associated with SOLVER-DESIGNATOR"))

(defgeneric solver-action (solver-designator)
  (:documentation "Gets the action name for this solver"))

(defgeneric solver-options (solver-designator)
  (:documentation "Gets the list of options taken by SOLVER-DESIGNATOR. Returns a list
of SOLVER-OPTION structures.")
  (:method-combination append))

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
(defmacro define-solver-metadata (class-or-keyword
                                  &key name symbol action description
                                    options spec-transformer)
  (let ((specializer (if (keywordp class-or-keyword)
                         `(eql ,class-or-keyword)
                         `,class-or-keyword)))
    `(progn
       (defmethod solver-name ((solver ,specializer)) ,name)
       (defmethod solver-symbols ((solver ,specializer))
         ,(if (listp symbol) `(list ,@symbol) `(list ,symbol)))
       (defmethod solver-description ((solver ,specializer)) ,description)
       (defmethod solver-action ((solver ,specializer)) ,action)
       (defmethod solver-options append ((solver ,specializer)) ,options)
       ,@(when spec-transformer
           (list
            `(defmethod transform-specification ((solver ,specializer) spec ctx)
               (funcall ,spec-transformer spec ctx)))))))

;;;
;;; Solving API
;;;
(defgeneric transform-specification (solver-designator specification context)
  (:documentation "Transforms SPECIFICATION into a format usable by a solver
designated by SOLVER-DESIGNATOR with the problem context CONTEXT. This function may
return NIL if the solver does not support problems with the given specification.")
  (:method (solver-designator specification context)
    "Default no-op method"
    (declare (ignore solver-designator context))
    specification))

(defgeneric smt-solver-configuration (solver-designator &key &allow-other-keys)
  (:documentation "Gets the SMT solver configuration desired by SOLVER-DESIGNATOR. A
core should always provide a default implementation of this method."))

(defgeneric initialize-solver (solver-designator &key &allow-other-keys)
  (:documentation "Sets up the solver designated by SOLVER-DESIGNATOR with options")
  (:method (solver-designator &key &allow-other-keys)
    (declare (ignore solver-designator))
    nil))

(defgeneric solve-problem (solver-designator semgus-problem &key &allow-other-keys)
  (:documentation "Solves SEMGUS-PROBLEM with a solver designated by SOLVER-DESIGNATOR.
Solver options, as returned by SOLVER-OPTIONS, will be passed as keywords."))
