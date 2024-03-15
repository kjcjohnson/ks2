;;;;
;;;; Configuration objects
;;;;
(in-package #:com.kjcjohnson.ks2.utilities)

(defclass config ()
  ((primary :reader primary
            :initarg :primary
            :type (or string keyword null)
            :documentation "Primary configuration key")
   (raw :reader raw
        :initarg :raw
        :type string
        :documentation "The raw configuration string")
   (options :reader options
            :initarg :options
            :type hash-table
            :documentation "Table of key-value option pairs"))
  (:documentation "Base configuration class"))

(defmethod initialize-instance :after ((config config) &key option-string)
  "Initializes CONFIG with option string OPTION-STRING"
  (multiple-value-bind (primary options)
      (parse-option-string option-string)
    (unless (and (null primary) (slot-boundp config 'primary))
      (setf (slot-value config 'primary) primary))
    (setf (slot-value config 'raw) option-string)
    (setf (slot-value config 'options) (*:alist-hash-table options :test 'equal))))

(defmethod print-object ((config config) stream)
  "Prints a config object"
  (print-unreadable-object (config stream)
    (format stream "CFG: ~a { " (primary config))
    (*:do-hash-table (k v (options config))
      (format stream "~a=~s " k v))
    (write-char #\} stream)))
