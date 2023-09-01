;;;;
;;;; Solvers and machinery
;;;;
(in-package #:com.kjcjohnson.ks2.solver-api)

(defclass solver ()
  ()
  (:documentation "The top-level solver class"))

;;;
;;; Global options and functionality
;;;
(defmethod solver-options append ((solver solver))
  "Gets global solver options"
  (list
   (make-solver-option
    :keyword :trace
    :name "Trace execution"
    :description "Write trace files with the executed programs"
    :type :boolean
    :default nil)))
