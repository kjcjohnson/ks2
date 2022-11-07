;;;;
;;;; Benchmarking code
;;;;
(in-package #:com.kjcjohnson.ks2.runner)

(defun start-solver (child-lisp problem-file solver)
  "Starts a solver and returns a promise for the result."
  (ecase solver
    (:enum (enum-solve/promise child-lisp problem-file :max-depth 20))
    (:duet (duet-solve/promise child-lisp problem-file :depth 20))
    (:frangel (frangel-solve/promise child-lisp problem-file))
    (:tde (tde-solve/promise child-lisp problem-file))))

(defun internal-time-to-seconds (internal-time)
  "Converts internal time to seconds."
  (/ internal-time internal-time-units-per-second))

(defun bytes-to-mebibytes (bytes)
  (/ bytes (* 1024 1024)))

(defparameter *symbol-timeout* "⌛")
(defparameter *symbol-error* "💥")
(defparameter *symbol-memory* "📈")
(defparameter *symbol-unknown* "❓")
(defparameter *symbol-no-solution* "🚫")

(defun get-metric (list what &key decorative)
  (if (consp list)
      (ecase what
        (:solved? (first list))
        (:time (cond ((and (null (first list)) decorative)
                      *symbol-timeout*)
                     ((and (first list) (string= (fourth list) "NIL") decorative)
                      *symbol-no-solution*)
                     (t
                      (format nil "~,2fs" (second list)))))
        (:memory (format nil "~,3fMiB" (third list)))
        (:result (fourth list))
        (:exec-rate (format nil "~,0fp/s" (fifth list))))
      (if decorative
          (case list
            (:error *symbol-error*)
            (:crash *symbol-memory*)
            (otherwise *symbol-unknown*))
          list)))

(defun get-metric-for-solver (list what solver &key decorative)
  (get-metric (cdr (assoc solver list)) what :decorative decorative))

(defun results-to-csv-string (results what &key decorative)
  (with-output-to-string (s)
    (format s "Benchmark,Enum,Duet,Frangel,TDE~%")
    (dolist (problem results)
      (format s
              "~s,~s,~s,~s,~s~%"
              (first problem)
              (get-metric-for-solver (cdr problem) what :enum
                                     :decorative decorative)
              (get-metric-for-solver (cdr problem) what :duet
                                     :decorative decorative)
              (get-metric-for-solver (cdr problem) what :frangel
                                     :decorative decorative)
              (get-metric-for-solver (cdr problem) what :tde
                                     :decorative decorative)))))

(defun write-all-results (results filename &key (if-exists :supersede))
  (with-open-file (fs filename :direction :output
                               :if-exists if-exists
                               :if-does-not-exist :create)
    (format fs "Solved?~%~a~%~%" (results-to-csv-string results :solved?))
    (format fs "Time~%~a~%~%" (results-to-csv-string results :time))
    (format fs "Peak Memory~%~a~%~%" (results-to-csv-string results :memory))
    (format fs "Execution Rate~%~a~%~%" (results-to-csv-string results :exec-rate))
    (format fs "Result~%~a~%~%" (results-to-csv-string results :result))
    (finish-output fs)))

(defun write-summary-results (results filename &key (if-exists :supersede))
  "Writes a summary of benchmark performance."
  (with-open-file (fs filename :direction :output
                               :if-exists if-exists
                               :if-does-not-exist :create)
    (format fs "~&Summary~%~a~%~%" (results-to-csv-string results :time
                                                        :decorative t))
    (finish-output fs)))

(defun write-suite-results (results filename &key summary)
  (with-open-file (fs filename :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
    (format fs "Results~%")
    (finish-output fs))

  (dolist (suite-result results)
    (with-open-file (fs filename :direction :output
                                 :if-exists :append
                                 :if-does-not-exist :create)
      (format fs "~&~%Suite,~a~%" (car suite-result))
      (finish-output fs))

    (if summary
        (write-summary-results (cdr suite-result) filename :if-exists :append)
        (write-all-results (cdr suite-result) filename :if-exists :append))))

(defun %run-benchmark (problem-file solver)
  "Runs a benchmark on a solver."
  (let (child-lisp)
    (unwind-protect
         (progn
           (setf child-lisp (swank-spawn))
           (bootstrap-tdp child-lisp)
           (force-gc child-lisp)
           (let* ((initial-memory (get-used-dynamic-space child-lisp))
                  (result-promise (start-solver child-lisp problem-file solver))
                  (start-time (get-internal-real-time))
                  (kill-time (+ (* 120 internal-time-units-per-second) start-time))
                  (max-memory 0))
             (loop until (lparallel:fulfilledp result-promise)
                   doing (let ((dm (get-used-dynamic-space child-lisp)))
                           (when (< max-memory dm)
                             (setf max-memory dm)))
                   while (< (get-internal-real-time) kill-time)
                   doing (sleep 1)
                   doing (format *trace-output* "."))

             (let* ((solved? (lparallel:fulfilledp result-promise))
                    (time (internal-time-to-seconds (- (get-internal-real-time)
                                                       start-time)))
                    (memory (- (bytes-to-mebibytes max-memory)
                               (bytes-to-mebibytes initial-memory)))
                    (result (if solved?
                                (ok-or-fail (lparallel:force result-promise))
                                nil))
                    (exec-count (get-execution-counter child-lisp))
                    (exec-rate (/ exec-count time)))


               (if solved?
                   (format t
                           "~&; RESULT: ~s~%;   TIME: ~,2fs~%;   MAX MEM OFFSET: ~,3fMiB~%;   PPS: ~,2fprog/s~%~%"
                         result
                         time
                         memory
                         exec-rate)
                   (format t
                           "~&; TIMEOUT after ~,2fs~%;   MAX MEM OFFSET: ~,3fMiB~%;   PPS: ~,2fprog/s~%~%"
                           time
                           memory
                           exec-rate))
               (list solved? time memory result exec-rate))))
    (unless (null child-lisp)
      (terminate-child child-lisp :urgent t)))))

(defparameter *solvers* '(:enum :duet :frangel))

(defparameter *problems* '("pp1a"
                           "pp2a"
                           "pp3a"
                           "easy_s"
                           "easy_r"
                           "dr-name"
                           "bikes"
                           "bv-easy"
                           "phone-9"
                           "PRE_ez"
                           "univ_1"
                           "univ_2"))

(defparameter *suites* '("classic"
                         "datatypes"
                         "imperative"
                         "integer-arithmetic"
                         "non-deterministic"
                         "regular-expressions/manually-constructed"
                         "regular-expressions/alpharegex"))

(defun ensure-ends-with (string suffix)
  "Makes sure STRING ends with SUFFIX"
  (if (str:ends-with? suffix string)
      string
      (concatenate 'string string suffix)))

(defun qualify-problem (problem)
  (if (pathnamep problem)
      problem
      (merge-pathnames (ensure-ends-with problem ".sexpr")
                       #P"D:/temp/benchmarks/")))

(defun qualify-suite (suite suite-root)
  (if (pathnamep suite)
      suite
      (merge-pathnames (make-pathname :directory `(:relative ,suite))
                       suite-root)))

(defun run-benchmark (problem solver)
  (handler-case
      (%run-benchmark (qualify-problem problem) solver)
    (rpc-error () :error)
    (swank-crash () :crash)
    (error (e)
      (format *trace-output*
              "; **OTHER CONDITION: ~s~%"
              e)
      :other-error)))

(defun run-suite (suite &optional (solver t) (suite-root #P"D:/temp/benchmarks/"))
  "Runs a suite. If SOLVER is T, runs all solvers, otherwise whatever is passed."
  (let ((solvers (if (eql t solver) *solvers* (list solver))))
    (loop for problem in (uiop:directory-files (qualify-suite suite suite-root))
          doing (format t "+++++ ~s +++++~%~%" problem)
          collecting
          (cons (pathname-name problem)
                (loop for solver in solvers
                      doing (format t "  --> ~a~%" solver)
                      collecting
                      (cons solver (run-benchmark problem solver)))))))

(defun run-all-suites ()
  "Runs all suites."
  (loop for suite in *suites*
        doing (format t "===== ~s =====~%~%" suite)
        collecting
        (cons suite (run-suite suite))))

(defun run-all-benchmarks ()
  "Runs all benchmarks"
  (loop for problem in *problems*
        doing (format t "+++++ ~s +++++~%~%" problem)
        collecting
        (cons problem
              (loop for solver in *solvers*
                    doing (format t "  --> ~a~%" solver)
                    collecting
                    (cons solver (run-benchmark problem solver))))))

