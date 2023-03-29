;;;;
;;;; Data structures and handling for solving results
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(defclass problem-result ()
  ((status :reader status
           :initarg :status
           :type (member :solved       ; The problem was successfully solved
                         :timeout      ; The problem timed out
                         :crash        ; The solver core crashed (usu. memory)
                         :unsupported  ; The problem is not supported by the solver
                         :unrealizable ; The problem is unrealizable by the solver
                         :error        ; The solver encountered an error
                         :unknown)
           :documentation "Status of the problem solution")
   (name :reader name
         :initarg :name
         :type string
         :documentation "Name of the problem")
   (solver :reader solver
           :initarg :solver
           :type string
           :documentation "Solver that solved this problem")
   (run-time :reader run-time
             :initarg :run-time
             :type number
             :documentation "Time (in seconds) it took the solver to produce a result")
   (peak-memory :reader peak-memory
                :initarg :peak-memory
                :type number
                :documentation "Max memory (in MiB) used above the baseline")
   (program :reader program
            :initarg :program
            :type (or string null)
            :documentation "The found program, or NIL if no program produced")
   (verify-rate :reader verify-rate
                :initarg :verify-rate
                :type number
                :documentation "Rate (in prog/s) that candidate programs were checked")
   (specification-types :reader specification-types
                        :initarg :specification-types
                        :type list
                        :documentation "List of specification type names (strings)"))
  (:documentation "Results from running a problem."))

(defun make-problem-result
    (name solver solved? run-time peak-memory program verify-rate specification-types)
  "Makes problem results"
  (make-instance 'problem-result
                 :name (string name)
                 :solver (string solver)
                 :run-time run-time
                 :peak-memory peak-memory
                 :program program
                 :verify-rate verify-rate
                 :specification-types specification-types
                 :status (cond
                           ((eql program :unsupported) :unsupported)
                           ((and solved? (string= program "NIL")) :unrealizable)
                           ((and (not solved?)) :timeout)
                           (solved? :solved))))

(defun make-problem-result-for-crash (name solver)
  "Makes a problem result for a crashed solver run"
  (make-instance 'problem-result
                 :name (string name)
                 :solver (string solver)
                 :status :crash))

(defun make-problem-result-for-error (name solver)
  "Makes a problem result for an erroring solver run"
  (make-instance 'problem-result
                 :name (string name)
                 :solver (string solver)
                 :status :error))

(defun make-problem-result-for-unknown (name solver)
  "Makes a problem result for an unknown error"
  (make-instance 'problem-result
                 :name (string name)
                 :solver (string solver)
                 :status :unknown))

(defun print-result (result)
  "Prints the result for terminal viewing"
  (case (status result)
    (:solved
     (format t
             "~&; RESULT: ~a~%;   TIME: ~,2fs~%;   MAX MEM OFFSET: ~,3fMiB~%;   PPS: ~,2fprog/s~%;   SPEC: ~{~a~^, ~}~%~%"
             (program result)
             (run-time result)
             (peak-memory result)
             (verify-rate result)
             (specification-types result)))
    (:timeout
     (format t
             "~&; TIMEOUT after ~,2fs~%;   MAX MEM OFFSET: ~,3fMiB~%;   PPS: ~,2fprog/s~%~%"
             (run-time result)
             (peak-memory result)
             (verify-rate result)))
    (t
     (format t "~&~a~%~%" (status result)))))

(defparameter *symbol-timeout* "⌛")
(defparameter *symbol-error* "💥")
(defparameter *symbol-memory* "📈")
(defparameter *symbol-unknown* "❓")
(defparameter *symbol-no-solution* "🚫")
(defparameter *symbol-unsupported* "⛔")

(defun problem-result-summary (result)
  "Gets a summary string for RESULT"
  (declare (type problem-result result))
  (ecase (status result)
    (:solved (format nil "~,2fs" (run-time result)))
    (:timeout *symbol-timeout*)
    (:crash *symbol-memory*)
    (:unsupported *symbol-unsupported*)
    (:unrealizable *symbol-no-solution*)
    (:error *symbol-error*)
    (:unknown *symbol-unknown*)))

(defclass suite-results ()
  ((suite :reader suite
          :initarg :suite
          :type suite
          :documentation "The suite that these results are for")
   (solvers :reader solvers
            :initarg :solvers
            :type (vector string)
            :documentation "Solvers that were run on this suite")
   (problems :reader problems
             :initarg :problems
             :type (vector string)
             :documentation "Problems that were run in this suite")
   (result-matrix :reader result-matrix
                  :initarg :result-matrix
                  :type (array (or problem-result null) 2)
                  :documentation "Matrix of solver/problem results")
   (summary-matrix :reader summary-matrix
                   :initarg :summary-matrix
                   :type (array string 2)
                   :documentation "Matrix of summary strings"))
  (:documentation "Results of running a suite of benchmarks"))

(defun make-suite-results (suite solvers)
  "Creates an empty suite results object for SOLVERS and PROBLEMS, which should be
sequences of string designators."
  (declare (type sequence solvers)
           (type suite suite))
  (let ((solver-dim (length solvers))
        (problem-dim (length (problems suite))))
    
    (make-instance 'suite-results
                   :suite suite
                   :solvers (map '(vector string) #'string solvers)
                   :problems (map '(vector string) #'name (problems suite))
                   :result-matrix (make-array (list problem-dim solver-dim)
                                              :element-type '(or problem-result null)
                                              :initial-element nil)
                   :summary-matrix (make-array (list problem-dim solver-dim)
                                               :element-type 'string
                                               :initial-element ""))))

(deftype %matrix-index ()
  "Acceptable types for indices for the suite result matrix"
  '(or fixnum problem string character symbol))

(defun %compute-matrix-index (index key-vector)
  "Computes a matrix index"
  (declare (type %matrix-index index)
           (type (vector string) key-vector))
  (cond
    ((numberp index) index)
    ((typep index 'problem) (%compute-matrix-index (name index) key-vector))
    (t (position (string index) key-vector :test #'string=))))

(defun suite-result (results solver problem)
  "Gets the results for SOLVER and PROBLEM"
  (declare (type suite-results results)
           (type %matrix-index solver problem))
  (aref (result-matrix results)
        (%compute-matrix-index problem (problems results))
        (%compute-matrix-index solver (solvers results))))

(defun (setf suite-result) (result results solver problem)
  "Sets the results for SOLVER and PROBLEM"
  (declare (type problem-result result)
           (type suite-results results)
           (type %matrix-index solver problem))
  (let ((solver-ix (%compute-matrix-index solver (solvers results)))
        (problem-ix (%compute-matrix-index problem (problems results))))
    (setf (aref (result-matrix results) problem-ix solver-ix)
          result)
    (setf (aref (summary-matrix results) problem-ix solver-ix)
          (problem-result-summary result))))