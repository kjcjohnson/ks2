;;;;
;;;; CLI package
;;;;
(defpackage #:com.kjcjohnson.ks2.cli
  (:use #:cl)
  (:local-nicknames (#:u #:com.kjcjohnson.ks2.utilities)
                    (#:sv #:com.kjcjohnson.ks2.solving)
                    (#:ks2 #:com.kjcjohnson.ks2)
                    (#:v #:org.shirakumo.verbose)
                    (#:jzon #:com.inuoe.jzon)
                    (#:gp #:eazy-gnuplot))
  (:export #:main #:generate-documentation))
