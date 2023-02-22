;;;;
;;;; Root ks2 system definitions
;;;;
(asdf:defsystem "com.kjcjohnson.ks2"
  :depends-on ("str"
               "alexandria")
  :pathname "src"
  :components ((:file "package")
               (:file "parser-interface" :depends-on ("package"))))

(asdf:defsystem "com.kjcjohnson.ks2/solver-api"
  :pathname "src/solver-api"
  :components ((:file "package")
               (:file "solver-api" :depends-on ("package"))))
