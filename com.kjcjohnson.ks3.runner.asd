;;;;
;;;; System definition for the ks3 runner
;;;;
(asdf:defsystem "com.kjcjohnson.ks3.runner"
  :depends-on ("swank-protocol"
               "uiop"
               "bordeaux-threads"
               "lparallel"
               "com.kjcjohnson.ks3.runner/helper")
  :serial t
  :components ((:file "package")
               (:file "child-lisp")
               (:file "runner")
               (:file "client-api")
               (:file "benchmarking")))

(asdf:defsystem "com.kjcjohnson.ks3.runner/helper"
  :depends-on ("com.kjcjohnson.tdp/test"
               "swank"
               "trivial-garbage")
  :components ((:file "swank-helper")))
