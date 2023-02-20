;;;;
;;;; Root ks2 system definitions
;;;;
(asdf:defsystem "com.kjcjohnson.ks2")

(asdf:defsystem "com.kjcjohnson.ks2/solver-api"
  :pathname "src/solver-api"
  :components ((:file "package")
               (:file "solver-api" :depends-on ("package"))))
