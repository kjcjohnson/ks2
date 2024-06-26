;;;;
;;;; System definition for the ks2 runner
;;;;
(asdf:defsystem "com.kjcjohnson.ks2.runner"
  :depends-on ("com.kjcjohnson.ks2/parser"
               "swank-protocol"
               "marshal"
               "uiop"
               "bordeaux-threads"
               "lparallel"
               "unix-opts"
               "verbose"
               "with-user-abort"
               "str")
  :build-operation "program-op"
  :build-pathname "bin/ks2"
  :entry-point "COM.KJCJOHNSON.KS2.RUNNER:MAIN"
  :serial t
  :components ((:module "runner"
                :pathname "src/runner"
                :serial t
                :components ((:file "package")
                             (:file "child-lisp")
                             (:file "runner")
                             (:file "client-api")
                             (:file "benchmarking")
                             (:file "cli-main")))))

(asdf:defsystem "com.kjcjohnson.ks2.runner/helper"
  :depends-on ("com.kjcjohnson.tdp/ks2"
               "com.kjcjohnson.ks2/solver-api"
               "com.kjcjohnson.ks2/utilities"
               "swank"
               "trivial-garbage")
  :build-operation "image-op"
  :build-pathname "bin/ks2-core"
  :serial t
  :components ((:module "helper"
                :pathname "src/helper"
                :serial t
                :components ((:file "swank-helper")))))
