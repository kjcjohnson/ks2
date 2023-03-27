;;;;
;;;; Pathname utilities
;;;;
(in-package #:com.kjcjohnson.ks2.utilities)

(defun rationalize-namestring (namestring &key (directory nil))
  "Converts a string NAMESTRING into a pathname to be used by ks2. The resultant
pathname will be absolute and truenamized."
  (declare (type string namestring))
  (uiop:ensure-pathname namestring
                        :namestring :native
                        :defaults (uiop:get-pathname-defaults)
                        :ensure-absolute t
                        :ensure-directory directory
                        :truenamize t))
