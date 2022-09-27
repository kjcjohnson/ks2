;;;;
;;;; Package definitions for the ks3 runner
;;;;
(defpackage #:com.kjcjohnson.ks3.runner
  (:use #:cl)
  (:local-nicknames (#:helper #:com.kjcjohnson.ks3.runner.helper))
  (:export #:init-and-start-swank)
  (:import-from #:swank
                #:defslimefun))
