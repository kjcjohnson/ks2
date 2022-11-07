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
    ((string= text "enum")
     :enum)
    ((string= text "tde")
     :tde)
    ((string= text "random")
     :random)
    (t
     (error 'enum-arg-parser-failed
             :raw-arg text
             :option :solver
             :available-options
             '("enum"
               "duet"
               "frangel"
               "tde"
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
   :short #\u
   :long "suite"
   :meta-var "SUITE"))

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

(defun main ()
  "Main CLI entrypoint"

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

    (format t "Options: ~a~%" options)
    
    (when-option (options :help)
      (print-help)
      (uiop:quit))

    (format t "~&Selected solver: ~a~%"
            (getf options :solver))

    (format t "~&Selected suite: ~a~%"
            (getf options :suite))

    (when-option (options :suite)
      (let ((suite-data (run-suite (getf options :suite)
                                   (getf options :solver)))
            (outname (substitute #\_ #\/ (getf options :suite))))
        (write-suite-results suite-data
                             (concatenate 'string "data/" outname ".all.csv"))
        (write-suite-results suite-data
                             (concatenate 'string "data/" outname ".sum.csv")
                             :summary t)))

    (unless (null free-args)
      (format t "~&Benchmark: ~a~%" free-args)

      (run-benchmark (first free-args) (getf options :solver)))))
