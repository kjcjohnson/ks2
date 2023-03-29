;;;;
;;;; Main program entry for the ks2 runner
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

(defun %normalize-solver (solver-string)
  ;; Technically we need to look this up in the core...
  (str:string-case (str:downcase solver-string)
    ("tde" :tde)
    ("top-down-enum" :tde)
    ("enum" :enum)
    ("duet" :duet)
    ("frangel" :frangel)
    ("random" :random)
    (otherwise (error "Invalid solver: ~a" solver-string))))

;;;
;;; Common options
;;;
(defun %option/core ()
  "Option for specifying the solver core to use"
  (clingon:make-option
   :string
   :description "core to use"
   :short-name #\c
   :long-name "core"
   :key :core
   :env-vars '("KS2_CORE")))

;;;
;;; Solve command
;;;
(defun solve/options ()
  "Options for solving problems"
  (list
   (clingon:make-option
    :string
    :description "solver to use"
    :short-name #\s
    :long-name "solver"
    :key :solver
    :env-vars '("KS2_SOLVER"))
   (%option/core)))

(defun solve/handler (cmd)
  "Handler for solving problems"
  (let ((solver (clingon:getopt cmd :solver))
        (problems (clingon:command-arguments cmd)))
    (when (zerop (length problems))
      (format *error-output* "~&error: no problems to solve specified~%")
      (clingon:exit 1))
    (invoke-solve (%normalize-solver solver) problems)))

(defun solve/command ()
  "Command for solving individual SemGuS problems"
  (clingon:make-command
   :name "solve"
   :description "solves an individual synthesis problem"
   :usage "[options] [benchmark]"
   :options (solve/options)
   :handler #'solve/handler))

;;;
;;; Benchmark command
;;;
(defun benchmark/options ()
  "Options for benchmarking"
  (list
   (clingon:make-option
    :list
    :description "solver to use (multiple allowed)"
    :short-name #\s
    :long-name "solver"
    :key :solvers
    :env-vars '("KS2_SOLVER"))
   (clingon:make-option
    :enum
    :description "output format for benchmark results"
    :short-name #\f
    :long-name "output-format"
    :key :output-format
    :initial-value "json"
    :env-vars '("KS2_OUTPUT_FORMAT")
    :items '(("json" . :json)
             ("csv" . :csv)))
   (clingon:make-option
    :filepath
    :description "output path for benchmark results"
    :short-name #\o
    :long-name "output"
    :key :output-path
    :initial-value "data"
    :env-vars '("KS2_OUTPUT_PATH"))
   (%option/core)))

(defun benchmark/handler (cmd)
  "Handler for benchmarking solvers"
  (let ((solvers (map 'list #'%normalize-solver (clingon:getopt cmd :solvers)))
        (output-format (clingon:getopt cmd :output-format))
        (output-path (clingon:getopt cmd :output-path))
        (suite-dir (clingon:command-arguments cmd)))
    (if (= 1 (length suite-dir))
        (setf suite-dir (first suite-dir))
        (progn
          (format *error-output* "~&error: exactly one suite directory expected~%")
          (clingon:exit 1)))
    (invoke-benchmark suite-dir solvers
                      :output-format output-format
                      :output-path output-path)))
    
(defun benchmark/command ()
  "Command for running solvers against SemGuS benchmarks"
  (clingon:make-command
   :name "benchmark"
   :description "benchmarks solvers against problem suites"
   :usage "[options] [suite-path]"
   :options (benchmark/options)
   :handler #'benchmark/handler))

;;;
;;; Report generation
;;;
(defun report/options ()
  "Options for report generation"
  (list
   (clingon:make-option
    :filepath
    :description "HTML output file to write report into"
    :short-name #\o
    :long-name "output"
    :key :output-path
    :env-vars '("KS2_OUTPUT_FILE"))))

(defun report/handler (cmd)
  "Handler for the report generation command"
  (let ((output-file (clingon:getopt cmd :output-path))
        (json-files (clingon:command-arguments cmd)))
    (invoke-report json-files output-file)))

(defun report/command ()
  "Command for generating a report from benchmark data files"
  (clingon:make-command
   :name "report"
   :description "generate reports from benchmark data files"
   :usage "[options] [json-files]"
   :options (report/options)
   :handler #'report/handler))

;;;
;;; Root command
;;;
(defun ks2/options ()
  "Global options for ks2"
  (list
   (clingon:make-option
    :flag
    :description "enables extra debugging information"
    :long-name "debug"
    :key :debug
    :env-vars '("KS2_DEBUG"))))

(defun ks2/handler (cmd)
  "Handler for the main ks2 command"
  (declare (ignore cmd))
  (format t "Hello, world!~%"))

(defun ks2/sub-commands ()
  "Subcommands for ks2"
  (list
   (solve/command)
   (benchmark/command)
   (report/command)))

(defun ks2/command ()
  "The main ks2 command"
  (clingon:make-command
   :name "ks2"
   :description "the ks2 synthesizer suite"
   :version "0.1.0" ; TODO: load dynamically from somewhere else?
   :license "MIT"
   :options (ks2/options)
   :sub-commands (ks2/sub-commands)
   :handler #'ks2/handler))

(defun main ()
  "Main entrypoint to the ks2 runner"
  (let ((app (ks2/command)))
    (clingon:run app)))