;;;;
;;;; ks2 core definitions
;;;;
(asdf:defsystem "systems.duck.ks2.core"
  :depends-on ("serapeum"
               "closer-mop"
               "verbose"
               "com.kjcjohnson.ks2/utilities")
  :pathname "src/cores"
  :components ((:file "package")
               (:file "core" :depends-on ("package"))))

(asdf:defsystem "systems.duck.ks2.core/synthkit"
  :depends-on ("systems.duck.ks2.core"
               "com.kjcjohnson.ks2.runner")
  :pathname "src/cores/synthkit"
  :components ((:file "package")
               (:file "synthkit-core" :depends-on ("package"))))
