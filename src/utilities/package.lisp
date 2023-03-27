;;;;
;;;; Utilities package
;;;;
(defpackage #:com.kjcjohnson.ks2.utilities
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria))
  (:export #:path-variable
           #:core-location
           #:locate-file
           #:rationalize-namestring))
