;;;;
;;;; Merges report tables together
;;;;
(defpackage #:com.kjcjohnson.ks2.plugins.report-merge-tables
  (:use #:cl)
  (:local-nicknames (#:plug #:systems.duck.plugboard)
                    (#:cli #:com.kjcjohnson.ks2.cli)))

(in-package #:com.kjcjohnson.ks2.plugins.report-merge-tables)

(plug:defplugin report-merge-tables ()
  ()
  (:documentation "A plugin to merge report tables into one big table")
  (:extends (cli::reporter cli::report-style-provider)))

(defclass merge-tables-style (cli::report-style) ())

(defmethod cli::provide-report-style or ((provider report-merge-tables) name options)
  ;; Merging style
  (when (string-equal name "merge-tables")
    'merge-tables-style))

(defun maybe-merge-tables (tables)
  (let ((separate-tables nil)
        (merged-tables nil))
    (loop for table in tables
          if (find (find-class 'merge-tables-style) (cli::styles table) :key #'class-of)
            collect table into yes-merge
          else
            collect table into no-merge
          end
          finally (setf separate-tables no-merge
                        merged-tables yes-merge))

    (when (null merged-tables)
      (return-from maybe-merge-tables separate-tables))

    ;; Hack - make sure tables have the same stuff in them
    (let ((field-count (length (cli::field-descriptors (first merged-tables))))
          (solver-count (length (cli::solvers (first merged-tables))))
          (style-count (length (cli::styles (first merged-tables)))))

      (loop for table in merged-tables
            unless (and (= (length (cli::field-descriptors table)) field-count)
                        (= (length (cli::solvers table)) solver-count)
                        (= (length (cli::styles table)) style-count))
              do (error "Mismatch in table shape when merging"))

      (cons (make-instance 'cli::report-table
                           :title "Merged Table"
                           :solvers (cli::solvers (first merged-tables))
                           :styles (cli::styles (first merged-tables))
                           :fields (cli::field-descriptors (first merged-tables))
                           :rows (reduce #'(lambda (x y)
                                             (concatenate 'vector x y))
                                         merged-tables
                                         :key #'cli::rows))
            separate-tables))))

(defmethod cli::write-report :around ((reporter report-merge-tables) stream tables)
  (if (null tables)
      (call-next-method)
      (call-next-method reporter stream (maybe-merge-tables tables))))
