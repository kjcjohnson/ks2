;;;;
;;;; Solving package - for solving benchmarks
;;;;
(defpackage #:com.kjcjohnson.ks2.solving
  (:use #:cl)
  (:local-nicknames (#:ks2 #:com.kjcjohnson.ks2)
                    (#:runner #:com.kjcjohnson.ks2.runner)
                    (#:s-api #:com.kjcjohnson.ks2.solver-api)
                    (#:a #:alexandria)
                    (#:v #:org.shirakumo.verbose))
  ;; Problems
  (:export #:problem #:make-problem #:run-problem)

  ;; Suites
  (:export #:suite #:name #:problems #:make-suite-from-directory #:run-suite)

  ;; Solver configs
  (:export #:solver-config #:normalize-solver)

  ;; Results
  (:export #:suite #:solvers #:problems #:suite-result #:suite-results
           #:print-result #:status)

  ;; Misc
  (:export #:*core-output-stream* #:*quiet*))
