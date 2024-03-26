;;;;
;;;; Main program entry for the ks2 runner
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

;;;
;;; Logging
;;;
(defun configure-logging (cmd)
  "Configures logging for the command CMD"
  (let ((debug nil)
        (quiet nil))
    ;; The debug flag might be set at any command level,
    ;;  while CMD is specifically the top level. So traverse and see.
    (clingon:with-command-tree (node cmd)
      (when (and (clingon:opt-is-set-p node :debug)
                 (clingon:getopt node :debug))
        (setf debug t))
      (when (and (clingon:opt-is-set-p node :quiet)
                 (clingon:getopt node :quiet))
        (setf quiet t)))

    ;; define-pipe just adds a new segment, so remove the existing one first
    (stop-logger)
    (setf v:*global-controller* (make-instance 'v:controller))

    (v:define-pipe ()
      (v:level-filter :level (if debug :debug :info))
      (v:stream-faucet :output *error-output*))

    (when quiet
      (setf sv:*quiet* t)
      (setf sv:*core-output-stream* (make-broadcast-stream)))))

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

(defun %option/corelist ()
  "Option for specifying multiple cores to use"
  (clingon:make-option
   :list
   :description "core to use (multiple allowed)"
   :short-name #\c
   :long-name "core"
   :key :cores
   :env-vars '("KS2_CORE")))

(defun %option/timeout ()
  "Option for specifying a timeout"
  (clingon:make-option
   :integer
   :description "timeout for problem files"
   :short-name #\t
   :long-name "timeout"
   :key :timeout
   :initial-value 120
   :env-vars '("KS2_TIMEOUT")))

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
   (%option/timeout)
   (%option/core)))

(defun solve/handler (cmd)
  "Handler for solving problems"
  (let ((solver (clingon:getopt cmd :solver))
        (core (core:make-core-config (clingon:getopt cmd :core)))
        (timeout (clingon:getopt cmd :timeout))
        (problems (clingon:command-arguments cmd)))
    (when (null solver)
      (format *error-output* "~&error: no solver specified~%")
      (clingon:exit 1))
    (when (zerop (length problems))
      (format *error-output* "~&error: no problems to solve specified~%")
      (clingon:exit 1))
    (let* ((statuses (invoke-solve (sv:normalize-solver solver)
                                   core
                                   problems
                                   :timeout timeout))
           (first-nz (find 0 statuses :test-not #'=)))
      (when first-nz (clingon:exit first-nz)))))

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
   (%option/timeout)
   (%option/corelist)))

(defparameter *default-solvers* (list "bottom-up-enum"
                                      "top-down-enum"
                                      "fragment-search"
                                      "duet"))

(defun benchmark/handler (cmd)
  "Handler for benchmarking solvers"
  (let ((solvers (map 'list #'sv:normalize-solver
                      (clingon:getopt cmd :solvers *default-solvers*)))
        (cores (map 'list #'core:make-core-config
                    (clingon:getopt cmd :cores)))
        (output-format (clingon:getopt cmd :output-format))
        (output-path (clingon:getopt cmd :output-path))
        (suite-dir (clingon:command-arguments cmd))
        (timeout (clingon:getopt cmd :timeout)))
    (if (= 1 (length suite-dir))
        (setf suite-dir (first suite-dir))
        (progn
          (format *error-output* "~&error: exactly one suite directory expected~%")
          (clingon:exit 1)))
    ;; Validate cores and solvers
    ;; There must either be 0, 1, or as many cores as solvers
    ;; Unless there's only one solver, then multiple cores allowed
    (if (< 1 (length solvers))
        (cond
          ((zerop (length cores))
           (setf cores (map 'list #'core:make-core-config
                            (make-list (length solvers) :initial-element ""))))
          ((= 1 (length cores))
           (setf cores
                 (make-list (length solvers) :initial-element (first cores))))
          ((not (= (length cores) (length solvers)))
           (format *error-output* "~&error: solver and core list not equal length~%")
           (clingon:exit 1)))
        (cond
          ((zerop (length cores))
           (setf cores (list (core:make-core-config ""))))
          ((< 1 (length cores))
           (setf solvers
                 (make-list (length cores) :initial-element (first solvers))))))
    (format t "; CORES: ~a~%" cores)
    (invoke-benchmark suite-dir solvers cores
                      :output-format output-format
                      :output-path output-path
                      :timeout timeout)))

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
    :enum
    :description "reporter to use"
    :short-name #\r
    :long-name "reporter"
    :key :reporter
    :env-vars '("KS2_REPORTER")
    :initial-value "html"
    :items '(("html" . html-reporter)
             ("text" . text-reporter)
             ("cactus" . cactus-reporter)
             ("compare" . comparison-reporter)))
   (clingon:make-option
    :list
    :description "solvers to report on (multiple allowed)"
    :short-name #\s
    :long-name "solver"
    :key :solvers
    :env-vars '("KS2_SOLVER"))
   (clingon:make-option
    :list
    :description "field to display (multiple allowed)"
    :short-name #\f
    :long-name "field"
    :key :fields
    :env-vars '("KS2_FIELD"))
   (clingon:make-option
    :filepath
    :description "HTML output file to write report into"
    :short-name #\o
    :long-name "output"
    :key :output-path
    :env-vars '("KS2_OUTPUT_FILE"))
   (clingon:make-option
    :list
    :description "report style configuration options (multiple allowed)"
    :short-name #\y
    :long-name "style"
    :key :styles
    :env-vars '("KS2_REPORT_STYLE"))))

(defun report/handler (cmd)
  "Handler for the report generation command"
  (let ((output-file (clingon:getopt cmd :output-path))
        (reporter (clingon:getopt cmd :reporter))
        (fields (clingon:getopt cmd :fields))
        (solvers (clingon:getopt cmd :solvers))
        (styles (clingon:getopt cmd :styles))
        (json-files (clingon:command-arguments cmd)))
    (invoke-report reporter json-files output-file
                   (or fields '("summary")) solvers styles)))

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
    :persistent t
    :description "enables extra debugging information"
    :long-name "debug"
    :key :debug
    :env-vars '("KS2_DEBUG"))
   (clingon:make-option
    :flag
    :persistent t
    :description "disables printing unnecessary information"
    :long-name "quiet"
    :short-name #\q
    :key :quiet)))

(defun ks2/handler (cmd)
  "Handler for the main ks2 command"
  (clingon:print-usage cmd *standard-output*))

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
   :handler #'ks2/handler
   :pre-hook #'configure-logging))

(defun main ()
  "Main entrypoint to the ks2 runner"
  (let ((app (ks2/command))
        (*features* (remove :swank *features*))) ; So CLINGON:EXIT works
    (clingon:run app)))

(defun generate-documentation (filename)
  "Prints documentation about ks2 to FILENAME"
  (if (string= filename "-")
      (clingon:print-documentation :markdown (ks2/command) *standard-output*)
      (progn
        (ensure-directories-exist filename)
        (with-open-file (out filename :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create)
          (clingon:print-documentation :markdown (ks2/command) out)))))
