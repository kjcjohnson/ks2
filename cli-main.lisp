;;;;
;;;; cli-main.lisp - command line interface to the runner
;;;;
(in-package #:com.kjcjohnson.ks2.runner)

(define-condition enum-arg-parser-failed (opts:arg-parser-failed)
  ((available-options
    :initarg :available-options
    :reader available-options
    :documentation "List of available options")))

(defun solver-parser (text)
  (setf text (string-downcase text))
  (cond
    ((string= text "frangel")
     :frangel)
    ((string= text "duet")
     :duet)
    ((or (string= text "enum") (string= text "bottom-up-enum"))
     :enum)
    ((string= text "top-down-enum")
     :tde)
    ((string= text "random")
     :random)
    (t
     (format *error-output*
             "Available solvers: ~{~a ~}"
             '("bottom-up-enum"
               #-ks2-public-release "duet"
               #-ks2-public-release "frangel"
               "top-down-enum"
               "random"))
     (error 'enum-arg-parser-failed
             :raw-arg text
             :option :solver
             :available-options
             '("bottom-up-enum"
               "duet"
               "frangel"
               "top-down-enum"
               "random")))))

(opts:define-opts
  (:name :help
   :description "Prints help about the ks2 runner"
   :short #\h
   :long "help")
  (:name :solver
   :description "Selects a solver to run"
   :short #\s
   :long "solver"
   :default t
   :arg-parser #'solver-parser
   :meta-var "SOLVER")
  (:name :suite
   :description "Selects a suite to run"
   :long "suite"
   :arg-parser #'identity
   :meta-var "SUITE")
  (:name :suite-root
   :description "Root directory of suite data"
   :long "suite-root"
   :arg-parser #'identity
   :meta-var "ROOT")
  (:name :debug
   :description "Enables extra tool debug information"
   :long "--debug"))

(defparameter *ks2-runner-debug* nil "Enables extra debug information about ks2")

(defmacro when-debug (&body body)
  `(when *ks2-runner-debug*
     ,@body))

;;;
;;; Some of this file is yoinked from the unix-opts examples
;;;
(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun print-help ()
  "Prints a help message"
  (opts:describe
   :prefix "ks2 - the ks2 synthesizer suite"
   :usage-of "ks2"
   :args "[BENCHMARK]"))

(defun fatal-msg (format &rest opts)
  (format *error-output* "~&fatal: ")
  (apply #'format *error-output* format opts)
  (format *error-output* "~%")
  (print-help)
  (opts:exit 1))

(defun debug-msg (format &rest opts)
  "Prints a debug message if the debug flag is set."
  (when-debug
    (format *error-output* "~&debug: ")
    (apply #'format *error-output* format opts)
    (format *error-output* "~%")
    (finish-output *error-output*)))

(defun main ()
  "Main CLI entrypoint"
  #+ks2-public-release
  (progn
    (format *error-output*
            "warning: This solver suite is currently experimental and prereleased.~%")
    (format *error-output*
            "warning:  - Command-line arguments will change in the future.~%")
    (format *error-output*
            "warning:  - The output format of this command is not stable.~%")
    (format *error-output*
            "warning:  - Synthesis algorithms are under active development.~%")
    (format *error-output*
            "warning: This tool is for evaluation and demonstration purposes only.~%"))

  (multiple-value-bind (options free-args)
      (handler-case
          (handler-bind ((opts:unknown-option #'unknown-option))
            (opts:get-opts))
        (opts:missing-arg (condition)
          (fatal-msg "option ~s needs an argument"
                     (opts:option condition)))
        (enum-arg-parser-failed (condition)
          (fatal-msg "cannot parse ~s as argument of ~s. Expected one of: ~{~a~^, ~}~%"
                     (opts:raw-arg condition)
                     (opts:option condition)
                     (available-options condition)))
        (opts:arg-parser-failed (condition)
          (fatal-msg "cannot parse ~s as argument of ~s~%"
                     (opts:raw-arg condition)
                     (opts:option condition)))
        (opts:missing-required-option (con)
          (fatal-msg "~a~%" con)))

    (setf *ks2-runner-debug* (getf options :debug))

    (debug-msg "Options: ~a~%" options)

    (when-option (options :help)
      (print-help)
      (uiop:quit))

    (debug-msg "~&Selected solver: ~a~%"
               (getf options :solver))


    (debug-msg "~&Selected suite: ~a~%"
               (getf options :suite))

    (when-option (options :suite)
      (let ((suite-data (run-suite (getf options :suite)
                                   (getf options :solver)
                                   (getf options :suite-root)))
            (outname (substitute #\_ #\/ (getf options :suite))))
        (write-all-results suite-data
                           (concatenate 'string "data/" outname ".all.csv"))
        (write-summary-results suite-data
                               (concatenate 'string "data/" outname ".sum.csv"))))

    (unless (null free-args)
      (format t "~&Benchmark: ~a~%" free-args)

      (run-benchmark (first free-args) (getf options :solver)))))
