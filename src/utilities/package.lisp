;;;;
;;;; Utilities package
;;;;
(defpackage #:com.kjcjohnson.ks2.utilities
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:* #:serapeum/bundle))
  (:export #:path-variable
           #:core-location
           #:locate-file
           #:locate-exe
           #:primary #:options
           #:config
           #:rationalize-namestring
           #:parse-option-string))
