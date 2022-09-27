;;;;
;;;; Benchmarking code
;;;;
(in-package #:com.kjcjohnson.ks3.runner)

(defun start-solver (child-lisp problem-file solver)
  "Starts a solver and returns a promise for the result."
  (ecase solver
    (:enum (enum-solve/promise child-lisp problem-file))
    (:duet (duet-solve/promise child-lisp problem-file))
    (:frangel (frangel-solve/promise child-lisp problem-file))))

(defun internal-time-to-seconds (internal-time)
  "Converts internal time to seconds."
  (/ internal-time internal-time-units-per-second))

(defun bytes-to-mebibytes (bytes)
  (/ bytes (* 1024 1024)))

(defun get-metric (list what)
  (if (consp list)
      (ecase what
        (:solved? (first list))
        (:time (format nil "~,2fs" (second list)))
        (:memory (format nil "~,3fMiB" (third list)))
        (:result (fourth list))
        (:exec-rate (format nil "~,0fp/s" (fifth list))))
      list))

(defun get-metric-for-solver (list what solver)
  (get-metric (cdr (assoc solver list)) what))

(defun results-to-csv-string (results what)
  (with-output-to-string (s)
    (format s "Benchmark,Enum,Duet,Frangel~%")
    (dolist (problem results)
      (format s
              "~s,~s,~s,~s~%"
              (first problem)
              (get-metric-for-solver (cdr problem) what :enum)
              (get-metric-for-solver (cdr problem) what :duet)
              (get-metric-for-solver (cdr problem) what :frangel)))))

(defun write-all-results (results filename)
  (with-open-file (fs filename :direction :output
                               :if-exists :overwrite
                               :if-does-not-exist :create)
    (format fs "Solved?~%~a~%~%" (results-to-csv-string results :solved?))
    (format fs "Time~%~a~%~%" (results-to-csv-string results :time))
    (format fs "Peak Memory~%~a~%~%" (results-to-csv-string results :memory))
    (format fs "Execution Rate~%~a~%~%" (results-to-csv-string results :exec-rate))
    (format fs "Result~%~a~%~%" (results-to-csv-string results :result))))
    

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
                           "; RESULT: ~s~%;   TIME: ~,2fs~%;   MAX MEM OFFSET: ~,3fMiB~%;   PPS: ~,2fprog/s~%~%"
                         result
                         time
                         memory
                         exec-rate)
                   (format t
                           "; TIMEOUT after ~,2fs~%;   MAX MEM OFFSET: ~,3fMiB~%;   PPS: ~,2fprog/s~%~%"
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

(defun qualify-problem (problem)
  (merge-pathnames (concatenate 'string problem ".sexpr")
                   #P"D:/temp/"))

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
              
