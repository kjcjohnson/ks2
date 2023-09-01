;;;;
;;;; Utilities for parsing and handling option strings
;;;;
(in-package #:com.kjcjohnson.ks2.utilities)

(defun parse-option-string (option-string)
  "Parses OPTION-STRING. Returns the primary piece and option alist as values"
  (let* ((pieces (str:split #\; option-string))
         (primary (first pieces))
         (options nil))
    (loop for opt in (cdr pieces)
          for (key value) = (str:split #\= opt :limit 2)
          when (null value)
            do (cond
                 ((str:starts-with? "-" key)
                  (setf key (str:trim key :char-bag "-")
                        value nil))
                 ((str:starts-with? "+" key)
                  (setf key (str:trim key :char-bag "+")
                        value t))
                 (t
                  (setf value t)))
          end
          do (push (cons key value) options))
    (values primary options)))
