;;;;
;;;; Takes the median of tables
;;;;
(defpackage #:com.kjcjohnson.ks2.plugins.report-median-tables
  (:use #:cl)
  (:local-nicknames (#:plug #:systems.duck.plugboard)
                    (#:cli #:com.kjcjohnson.ks2.cli)))

(in-package #:com.kjcjohnson.ks2.plugins.report-median-tables)

(plug:defplugin report-median-tables ()
  ()
  (:documentation "A plugin to take the median of report tables")
  (:extends (cli::reporter cli::report-style-provider)))

(defclass median-tables-style (cli::report-style) ())

(defmethod cli::provide-report-style or ((provider report-median-tables) name options)
  ;; Merging style
  (when (string-equal name "median-tables")
    'median-tables-style))

(defun %verify-valid-to-median (tables)
  "Makes sure the tables are the same shape"
  ;; Hack - make sure tables have the same stuff in them
  (let ((field-count (length (cli::field-descriptors (first tables))))
        (solver-count (length (cli::solvers (first tables))))
        (solvers (cli::solvers (first tables)))
        (style-count (length (cli::styles (first tables)))))

    (loop for table in tables
          unless (and (= (length (cli::field-descriptors table)) field-count)
                      (= (length (cli::solvers table)) solver-count)
                      (equalp solvers (cli::solvers table))
                      (= (length (cli::styles table)) style-count))
            do (error "Mismatch in table shape when taking median"))))

(defun median-entries (entries)
  (let* ((fe (first entries))
         (solver (cli::solver fe))
         (field-ix nil))
    (assert (every #'(lambda (e) (string= solver (cli::solver e))) entries))
    ;;
    ;; We pick a single field here to find the median of...very naively
    ;;
    (loop for fd across (cli::field-descriptors fe)
          for fd-ix from 0
          when (and (not (eql :status (cli::id fd)))
                    (not (eql :summary (cli::id fd))))
            do (setf field-ix fd-ix)
               (loop-finish))
    (assert field-ix)

    ;;
    ;; Then convert values, sort, find median, and map back to original entry
    ;;
    (let* ((values (map 'list #'(lambda (e)
                                  (let ((val (read-from-string
                                              (aref (cli::field-values e) field-ix))))
                                    (when (null val)
                                      (setf val 1d23))
                                    (check-type val number)
                                    val))
                        entries))
           (sorted (sort (copy-list values) #'<))
           (median-ix (floor (/ (length values) 2)))
           (median (elt sorted median-ix))
           (orig-ix (position median values)))
      (elt entries orig-ix))))

(defun median-rows (rows)
  (let ((benchmark (cli::benchmark (first rows))))
    (assert (every #'(lambda (r) (string= benchmark (cli::benchmark r))) rows))

    (make-instance
     'cli::report-row
     :benchmark benchmark
     :entries (coerce
               (loop for entry across (cli::entries (first rows))
                     for e-ix from 0
                     collecting (median-entries (map 'list #'(lambda (r)
                                                               (aref (cli::entries r)
                                                                     e-ix))
                                                     rows)))
               'vector))))

(defun median-table (tables)
  "Takes the median of each field in the tables and returns a new table"
  (%verify-valid-to-median tables)

  (let ((new-rows (make-array (length (cli::rows (first tables))))))

    (loop for row across (cli::rows (first tables))
          for row-ix from 0
          for benchmark = (cli::benchmark row)
          do (setf (aref new-rows row-ix)
                   (median-rows (map 'list #'(lambda (tab)
                                               (aref (cli::rows tab) row-ix))
                                     tables))))
    (make-instance 'cli::report-table
                   :title (cli::title (first tables))
                   :solvers (cli::solvers (first tables))
                   :styles (cli::styles (first tables))
                   :fields (cli::field-descriptors (first tables))
                   :rows new-rows)))

(defun maybe-median-tables (tables)
  "Takes the median of the given tables. Only works for numeric fields!"
  (let ((table-index (make-hash-table :test 'equal)))
    (loop for table in tables
          do (push table (gethash (cli::title table) table-index)))

    (loop for tables being the hash-values of table-index
          collecting (median-table tables))))

(defmethod cli::write-report :around ((reporter report-median-tables) stream tables)
  (if (or (null tables) (not (cli::get-report-style-option reporter "median-tables")))
      (call-next-method)
      (call-next-method reporter stream (maybe-median-tables tables))))
