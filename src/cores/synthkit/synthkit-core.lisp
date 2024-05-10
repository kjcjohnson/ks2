;;;;
;;;; Synthkit core
;;;;
(in-package #:systems.duck.ks2.core.synthkit)

(defclass synthkit-core (core:core)
  ((child-lisp :reader child-lisp
               :writer %set-child-lisp
               :initarg :child-lisp
               :documentation "The underlying SWANK child connection"))
  (:core-name "synthkit")
  (:metaclass core:core-class)
  (:documentation "The synthkit core"))

(defmethod core:core-options append ((core synthkit-core))
  "Gets options!"
  (list
   (core:make-core-option
    :keyword :program-compile
    :name "pc"
    :description "If true, use the program compiler"
    :type :boolean
    :default nil)
   (core:make-core-option
    :keyword :debug-compile
    :name "debug-compile"
    :type :boolean
    :default nil)
   (core:make-core-option
    :keyword :force-semgus-verifier
    :name "force-semgus-verifier"
    :type :boolean
    :default nil)
   (core:make-core-option
    :keyword :force-no-pbe-constraints
    :name "force-no-pbe-constraints"
    :type :boolean
    :default nil)))

(defmethod core:spawn-core ((core synthkit-core) &key output &allow-other-keys)
  "Spawns a synthkit core"
  (let ((child-lisp (runner::swank-spawn :output output)))
    (%set-child-lisp child-lisp core)
    (runner::set-core-options child-lisp (core:core-option-plist core))
    (runner::bootstrap-tdp child-lisp)))

(defmethod core:terminate-core ((core synthkit-core) &key urgent)
  "Terminates the synthkit core CORE"
  (unless (null (child-lisp core))
    (runner::terminate-child (child-lisp core) :urgent urgent)
    (%set-child-lisp nil core)))
