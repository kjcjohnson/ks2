;;;;
;;;; Root ks2 system definitions
;;;;
(asdf:defsystem "com.kjcjohnson.ks2"
  :depends-on ("str"
               "alexandria"
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
               (:file "path-utilities")))

(asdf:defsystem "com.kjcjohnson.ks2/solver-api"
  :pathname "src/solver-api"
  :components ((:file "package")
               (:file "solver-api" :depends-on ("package"))))
