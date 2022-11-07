;;;;
;;;; Package definitions for the ks2 runner
;;;;
(defpackage #:com.kjcjohnson.ks2.runner.helper
  (:export #:get-used-dynamic-space
           #:get-total-dynamic-space
           #:get-execution-counter
           #:force-gc
           #:bootstrap-tdp
           #:enum-solve
           #:duet-solve
           #:frangel-solve
           #:tde-solve))

(defpackage #:com.kjcjohnson.ks2.runner
  (:use #:cl)
  (:local-nicknames (#:helper #:com.kjcjohnson.ks2.runner.helper))
  (:export #:init-and-start-swank #:main)
  (:import-from #:swank
                #:defslimefun))
