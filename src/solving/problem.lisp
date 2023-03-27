;;;;
;;;; Problems
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(defclass problem ()
  ((name :reader name
         :initarg :name
         :type string
         :documentation "The friendly name of this problem")
   (path :reader path
         :initarg :path
         :type pathname
         :documentation "The pathname of this problem"))
  (:documentation "A problem and metadata"))

(defun make-problem (path)
  "Makes a problem object from PATH"
  (declare (type pathname path))
  (make-instance 'problem
                 :path path
                 :name (pathname-name path)))

(defmethod print-object ((obj problem) stream)
  "Prints OBJ to STREAM as a problem."
  (print-unreadable-object (obj stream :type t)
    (format stream "~a [~a]" (name obj) (path obj))))
