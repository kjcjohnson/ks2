;;;;
;;;; Package definitions for the ks2 runner
;;;;
(defpackage #:com.kjcjohnson.ks2.runner.helper
  (:export #:get-used-dynamic-space
           #:get-total-dynamic-space
           #:get-execution-counter
           #:get-statistics
           #:force-gc
           #:get-gc-run-time
           #:bootstrap-tdp
           #:enum-solve
           #:duet-solve
           #:frangel-solve
           #:tde-solve
           #:load-problem-file))

(defpackage #:com.kjcjohnson.ks2.runner
  (:use #:cl)
  (:local-nicknames (#:helper #:com.kjcjohnson.ks2.runner.helper))
  (:export #:init-and-start-swank #:main)
  (:import-from #:swank
                #:defslimefun))
