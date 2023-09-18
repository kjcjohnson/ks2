;;;;
;;;; Solver configuration
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(define-condition solver-config-error (simple-error) ()
  (:documentation "A condition thrown when a solver configuration is wrong"))

(defun solver-config-error (format &rest args)
  "Signals a solver-config-error"
  (error 'solver-config-error :format-control format :format-arguments args))

(defclass solver-config ()
  ((solver :accessor solver
           :initarg :solver
           :type (or string keyword)
           :documentation "Solver keyword to use")
   (raw :reader raw
        :initarg :raw
        :type string
        :documentation "The raw solver string")
   (name :reader name
         :initarg :name
         :type string
         :documentation "The name of this solver config (for use in reports, etc.)")
   (options :reader options
            :initarg :options
            :type list
            :documentation "Alist of solver options and values (to be parsed later)")
   (option-plist :accessor option-plist
                 :initarg :option-plist
                 :type list
                 :documentation "Plist of options to apply to solve command")))

(defmethod print-object ((sc solver-config) stream)
  "Prints a solver config"
  (print-unreadable-object (sc stream)
    (format stream "SOLVER-CONFIG[~s]" (solver sc))
    (unless (null (options sc))
      (format stream " ")
      (loop for ((key . value) . more) on (options sc)
            do (format stream "~a=~a~@[; ~]" key value more)))))

(defun normalize-solver (solver-string)
  ;; Technically we need to look this up in the core...
  (let ((pieces (str:split #\; (str:downcase solver-string)))
        (name solver-string)
        solver
        options)
    (setf solver (first pieces)) ; Delay parsing until we have a core connection
    (loop for opt in (cdr pieces)
          for (key value) = (str:split #\= opt :limit 2)
          when (null value)
            do (if (str:starts-with? "-" key)
                   (setf key (str:trim-left key :char-bag "-")
                         value nil)
                   (setf value t))
          end
          do (str:string-case key
               ("name" (setf name value))
               (otherwise (push (cons key value) options))))
    (make-instance 'solver-config :solver solver
                                  :options options
                                  :raw solver-string
                                  :name name)))

(defun %check-solver (child-lisp solver-config)
  "Checks a solver in a child lisp"
  (let ((normalized nil))
    (loop for designator in (runner::list-solvers child-lisp)
          for symbols = (runner::solver-symbols child-lisp designator)
          do (v:trace :solving "Designator: ~a, symbols: ~a" designator symbols)
          when (or (eql (solver solver-config) designator)
                   (find (solver solver-config) symbols :test #'string-equal))
            do (setf normalized designator))
    (if (null normalized)
        (solver-config-error "Invalid solver: ~a" (solver solver-config))
        (setf (solver solver-config) normalized))))

(defun validate-solver-options (child-lisp solver-config)
  "Checks solver options in a core"
  (%check-solver child-lisp solver-config)
  (flet ((parse-boolean-option (value)
           (cond
             ((null value) nil)
             (t
              (setf value (str:downcase value))
              (str:string-case value
                ("t" t)
                ("yes" t)
                ("true" t)
                ("nil" nil)
                ("no" nil)
                ("false" nil)
                (otherwise (solver-config-error
                            "Invalid Boolean solver option: ~a" value))))))
         (parse-number-option (value)
           (handler-case
               (parse-number:parse-number value)
             (parse-number:invalid-number ()
               (solver-config-error "Invalid Number solver option: ~a" value))))
         (self-or-first (x)
           (if (atom x) x (first x)))
         (parse-member-option (value type)
           (loop for opt in (rest type)
                 when (string-equal (string opt) (string value)) do
                   (return opt)
                 end
                 finally (solver-config-error
                          "Invalid Member solver option: ~a (not in ~a)"
                          value (rest type)))))

    (let ((available-options
            (runner::solver-options child-lisp (solver solver-config)))
          (opt-plist nil))
      (loop for (key . value) in (options solver-config)
            for opt = (find key available-options
                            :key (a:compose #'symbol-name
                                            #'s-api::solver-option-keyword)
                            :test #'string-equal)
            for type = (and opt (s-api::solver-option-type opt))
            unless opt
              do (solver-config-error "Invalid solver option: ~a" key)
            end
            do (push (s-api::solver-option-keyword opt) opt-plist)
               (push (ecase (self-or-first type)
                       (:boolean (parse-boolean-option value))
                       (:number (parse-number-option value))
                       (:member (parse-member-option value type)))
                     opt-plist))
      (setf (option-plist solver-config) (nreverse opt-plist)))))
