;;;;
;;;; Default plugin system
;;;;
(asdf:defsystem "com.kjcjohnson.ks2.plugins"
  :depends-on ("systems.duck.plugboard"
               "com.kjcjohnson.ks2/app")
  :serial t
  :components ((:file "report-style")
               (:file "report-merge-tables")
               (:file "report-median-tables")))
