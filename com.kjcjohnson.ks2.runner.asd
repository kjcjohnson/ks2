;;;;
;;;; System definition for the ks2 runner
;;;;
(asdf:defsystem "com.kjcjohnson.ks2.runner"
  :depends-on ("swank-protocol"
               "marshal"
               "uiop"
               "bordeaux-threads"
               "lparallel"
               "unix-opts"
               "com.kjcjohnson.ks2.runner/helper")
  :build-operation "program-op"
  :build-pathname "bin/ks2"
  :entry-point "COM.KJCJOHNSON.KS2.RUNNER:MAIN"
  :serial t
  :components ((:file "package")
               (:file "child-lisp")
               (:file "runner")
               (:file "client-api")
               (:file "benchmarking")
               (:file "cli-main")))

(asdf:defsystem "com.kjcjohnson.ks2.runner/helper"
  :depends-on ("com.kjcjohnson.tdp/test"
               "swank"
               "trivial-garbage")
  :components ((:file "swank-helper")))
