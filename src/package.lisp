;;;;
;;;; ks2 package for generic things
;;;;
(defpackage #:com.kjcjohnson.ks2
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:u #:com.kjcjohnson.ks2.utilities))
  (:export #:ensure-sexp-benchmark-file))
