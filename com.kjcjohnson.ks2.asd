;;;;
;;;; Root ks2 system definitions
;;;;
(asdf:defsystem "com.kjcjohnson.ks2"
  :depends-on ("str"
               "alexandria"
               "verbose"
               "parse-number"
               "systems.duck.ks2.core"
               "systems.duck.ks2.core/synthkit"
               "com.kjcjohnson.ks2/parser"
               "com.kjcjohnson.ks2/utilities"
               "com.kjcjohnson.ks2/solver-api")
  :pathname "src"
  :serial t
  :components ((:module "solving"
                :serial t
                :components ((:file "package")
                             (:file "solver-config")
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
  :depends-on ("str" "alexandria" "serapeum")
  :pathname "src/utilities"
  :serial t
  :components ((:file "package")
               (:file "option-strings")
               (:file "config")
               (:file "path-utilities")
               (:file "pathname-utilities")))

(asdf:defsystem "com.kjcjohnson.ks2/solver-api"
  :pathname "src/solver-api"
  :components ((:file "package")
               (:file "solver-api" :depends-on ("package"))
               (:file "solvers" :depends-on ("package" "solver-api"))))


(asdf:defsystem "com.kjcjohnson.ks2/app"
  :depends-on ("uiop"
               "clingon"
               "str"
               "shasht"
               "com.inuoe.jzon"
               "exit-hooks"
               "local-time"
               "eazy-gnuplot"
               "cl-csv"
               "systems.duck.ks2.core"
               "com.kjcjohnson.ks2.runner"
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
                             (:file "report")
                             (:file "main")
                             (:file "hooks")))))

(asdf:defsystem "com.kjcjohnson.ks2/app-all"
  :depends-on ("com.kjcjohnson.ks2/app"
               "com.kjcjohnson.ks2.plugins")
  :build-operation "program-op"
  :build-pathname "bin/ks2"
  :entry-point "COM.KJCJOHNSON.KS2.CLI:MAIN")
