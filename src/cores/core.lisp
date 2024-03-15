;;;;
;;;; Solver cores
;;;;
(in-package #:com.kjcjohnson.ks2.core)

;;;
;;; Metaclass for core discovery
;;;
(defclass core-class (mop:standard-class)
  ((core-name :reader core-name
              :initarg :core-name
              :type list))
  (:default-initargs :core-name nil)
  (:documentation "Metaclass for cores"))

(defmethod mop:validate-superclass ((class core-class) (superclass mop:standard-class))
  t)

;;;
;;; Core configuration
;;;
(defclass core-config (u:config)
  ()
  (:default-initargs :primary "synthkit")
  (:documentation "Core configuration class"))

(defun make-core-config (config-string)
  "Makes a core configuration from CONFIG-STRING"
  (declare (type (or null string) config-string))
  (make-instance 'core-config :option-string config-string))

;;;
;;; Core superclass
;;;
(defclass core ()
  ((config :reader core-config
           :initarg :config
           :type core-config
           :documentation "This core's configuration")
   (option-plist :reader core-option-plist
                 :initarg :option-plist
                 :type list
                 :documentation "The plist for passing options to core methods"))
  (:metaclass core-class)
  (:documentation "A solver core"))

;;;
;;; Core implementation protocol
;;;
(defgeneric spawn-core (core &key output &allow-other-keys)
  (:documentation "Spawns the core"))

(defgeneric terminate-core (core &key urgent &allow-other-keys)
  (:documentation "Terminates the core"))

(defgeneric core-options (core)
  (:method-combination append)
  (:method append ((core core)) (list))
  (:documentation "Gets a list of core options"))

(defclass core-option ()
  ((keyword :initarg :keyword
            :reader core-option-keyword)
   (name :initarg :name
         :reader core-option-name)
   (description :initarg :description
                :reader core-option-description)
   (type :initarg :type
         :reader core-option-type)
   (default :initarg :default
            :reader core-option-default))
  (:documentation "A core option"))

(defun make-core-option (&rest args &key keyword name description type default)
  "Makes a core option"
  (declare (ignore keyword name description type default))
  (apply #'make-instance 'core-option args))

(defun %process-core-value (value option)
  "Processes a value for a core option"
  (case (core-option-type option)
    (:boolean (cond ((null value) nil)
                    ((eq value t) t)
                    (t (setf value (str:downcase value))
                       (str:string-case value
                         ("t" t)
                         ("yes" t)
                         ("true" t)
                         ("nil" nil)
                         ("no" nil)
                         ("false" nil)
                         (otherwise (error "Bad Boolean core option: ~a" value))))))
    (otherwise (error "Option type not yet implemented: ~a" value))))

(defun process-core-options-into-plist (core core-config)
  "Processes core options"
  (let ((opts (core-options core))
        (ht (u:options core-config)))
    (loop for opt in opts
          for (val found) = (multiple-value-list (gethash (core-option-name opt) ht))
          if (and (not found) (slot-boundp opt 'default))
            collect (core-option-keyword opt)
            and collect (core-option-default opt)
          end
          if found
            collect (core-option-keyword opt)
            and collect (%process-core-value val opt))))

(defmethod initialize-instance :around ((core core) &rest args
                                        &key config &allow-other-keys)
  (let ((opt-plist (process-core-options-into-plist core config)))
    (apply #'call-next-method core :option-plist opt-plist (append opt-plist args))))


;;;
;;; Base implementations
;;;
(defun lookup-core-class (core-config)
  "Looks up a core class based on CORE-CONFIG"
  (let ((name (u:primary core-config)))
    (labels ((traverse-hierarchy (class)
               (if (and (typep class 'core-class)
                        (find name (core-name class) :test #'string-equal))
                   class
                   (loop for subclass in (mop:class-direct-subclasses class)
                         for result = (traverse-hierarchy subclass)
                         when result do (return result)))))
      (traverse-hierarchy (find-class 'core)))))

(defun start-core (core-config &key output)
  "Starts a core described by CORE-CONFIG with output to stream OUTPUT"
  (let* ((class (lookup-core-class core-config))
         (core (make-instance class :config core-config)))
    (apply #'spawn-core core :output output (core-option-plist core))
    core))

(defun stop-core (core &key urgent)
  "Stops CORE, optionally as soon as possible if URGENT is T"
  (apply #'terminate-core core :urgent urgent (core-option-plist core)))
