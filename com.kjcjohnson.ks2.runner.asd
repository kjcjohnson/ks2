;;;;
;;;; System definition for the ks2 runner
;;;;
(asdf:defsystem "com.kjcjohnson.ks2.runner"
  :depends-on ("com.kjcjohnson.ks2"
               "swank-protocol"
               "marshal"
               "uiop"
               "bordeaux-threads"
               "lparallel"
               "unix-opts"
               "str")
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
  :depends-on ("com.kjcjohnson.tdp/ks2"
               "com.kjcjohnson.ks2/solver-api"
               "swank"
               "trivial-garbage")
  :build-operation "image-op"
  :build-pathname "bin/ks2-runner"
  :serial t
  :components ((:file "swank-helper")))
