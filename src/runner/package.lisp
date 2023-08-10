;;;;
;;;; Package definitions for the ks2 runner
;;;;
(defpackage #:com.kjcjohnson.ks2.runner.helper
  (:export #:get-used-dynamic-space
           #:get-total-dynamic-space
           #:get-execution-counter
           #:get-checkpoint-times
           #:get-concrete-candidate-counter
           #:get-partial-candidate-counter
           #:get-concrete-candidates-by-size
           #:get-statistics
           #:force-gc
           #:get-gc-run-time
           #:bootstrap-tdp
           #:enum-solve
           #:duet-solve
           #:frangel-solve
           #:tde-solve
           #:load-problem-file)
  (:export #:list-solvers
           #:solver-name
           #:solver-symbols
           #:solver-description
           #:solver-action
           #:solver-options
           #:solve-problem))

(defpackage #:com.kjcjohnson.ks2.runner
  (:use #:cl)
  (:local-nicknames (#:helper #:com.kjcjohnson.ks2.runner.helper)
                    (#:ks2 #:com.kjcjohnson.ks2))
  (:export #:init-and-start-swank #:main #:main2)
  (:export #:get-execution-counter
           #:get-checkpoint-times
           #:get-concrete-candidate-counter
           #:get-partial-candidate-counter
           #:get-concrete-candidates-by-size)
  (:import-from #:swank
                #:defslimefun))
