;;;;
;;;; Root ks2 system definitions
;;;;
(asdf:defsystem "com.kjcjohnson.ks2"
  :depends-on ("str"
               "alexandria"
               "verbose"
               "com.kjcjohnson.ks2/parser"
               "com.kjcjohnson.ks2/utilities")
  :pathname "src"
  :serial t
  :components ((:module "solving"
                :serial t
                :components ((:file "package")
                             (:file "problem")
                             (:file "suite")
                             (:file "results")
                             (:file "run-problem")
                             (:file "run-suite")))))

(asdf:defsystem "com.kjcjohnson.ks2/parser"
  :depends-on ("alexandria"
               "com.kjcjohnson.ks2/utilities")
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "parser-interface" :depends-on ("package"))))

(asdf:defsystem "com.kjcjohnson.ks2/utilities"
  :depends-on ("str" "alexandria")
  :pathname "src/utilities"
  :serial t
  :components ((:file "package")
               (:file "path-utilities")
               (:file "pathname-utilities")))

(asdf:defsystem "com.kjcjohnson.ks2/solver-api"
  :pathname "src/solver-api"
  :components ((:file "package")
               (:file "solver-api" :depends-on ("package"))))


(asdf:defsystem "com.kjcjohnson.ks2/app"
  :depends-on ("uiop"
               "clingon"
               "str"
               "shasht"
               "exit-hooks"
               "com.kjcjohnson.ks2")
  :build-operation "program-op"
  :build-pathname "bin/ks2"
  :entry-point "COM.KJCJOHNSON.KS2.CLI:MAIN"
  :serial t
  :components ((:module "src/cli"
                :serial t
                :components ((:file "package")
                             (:file "solve")
                             (:file "benchmark")
                             (:file "main")
                             (:file "hooks")))))
