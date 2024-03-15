;;;;
;;;; Package for cores
;;;;
(defpackage #:com.kjcjohnson.ks2.core
  (:use #:cl)
  (:nicknames #:systems.duck.ks2.core)
  (:local-nicknames (#:u #:com.kjcjohnson.ks2.utilities)
                    (#:mop #:closer-mop)
                    (#:v #:org.shirakumo.verbose)
                    (#:* #:serapeum/bundle))
  (:export #:core-class #:core #:core-config #:make-core-config #:make-core-option
           #:spawn-core #:terminate-core #:core-options
           #:start-core #:stop-core))
