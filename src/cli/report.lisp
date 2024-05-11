;;;;
;;;; Report functionality
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

;;;
;;; Report data structures
;;;
;;;  Reports are divided into Tables, Rows, Entries, and Fields
;;;   - A table holds results for an entire suite
;;;   - A row holds results for a particular benchmark
;;;   - An entry holds a benchmark's fields for a particular solver
;;;   - A field holds a specific data point for a particular solver
(defclass report-field-descriptor ()
  ((id :reader id
       :initarg :id
       :type symbol
       :documentation "Identifer for what data this field is associated with")
   (name :reader name
         :initarg :name
         :type string
         :documentation "Name of this field")
   (location :reader location
             :initarg :location
             :type string
             :documentation "Where in the result matrix this field is found.")
   (format :reader field-format
           :initarg :field-format
           :type string
           :documentation "How to format this field")
   (transformer :reader field-transformer
                :initarg :transformer
                :type function
                :documentation "A function to transform the report data field prior to
passing to the formatting code")
   (options :reader field-options
            :initarg :options
            :type list ; alist
            :documentation "Alist of raw field options"))
  (:documentation "A specific point of data in a report"))

(defun report-field-descriptor-equal (rfd1 rfd2)
  "Checks if RFD1 is equal to RFD2"
  (and (eql (id rfd1) (id rfd2))
       (string= (name rfd1) (name rfd2))
       (string= (location rfd1) (location rfd2))
       (string= (field-format rfd1) (field-format rfd2))
       (eql (field-transformer rfd1) (field-transformer rfd2))
       (equal (field-options rfd1) (field-options rfd2))))

(defmethod print-object ((obj report-field-descriptor) stream)
  (print-unreadable-object (obj stream)
    (format stream "RFD: ~a" (id obj))))

(defun make-report-field-descriptor (id &key name location format transformer options)
  "Makes a field descriptor"
  (make-instance 'report-field-descriptor :id id
                                          :name name
                                          :location location
                                          :field-format format
                                          :transformer transformer
                                          :options options))

(defclass report-entry ()
  ((solver :accessor solver
           :initarg :solver
           :type string
           :documentation "Solver this entry is for")
   (field-descriptors :accessor field-descriptors
                      :initarg :field-descriptors
                      :type (vector report-field-descriptor)
                      :documentation "Fields in this entry")
   (field-values :accessor field-values
                 :initarg :field-values
                 :type (vector string)
                 :documentation "Values of fields in this entry")
   (style-field-descriptors :accessor style-field-descriptors
                            :initarg :style-field-descriptors
                            :type (vector report-field-descriptor)
                            :documentation "Extra fields for styles")
   (style-field-values :accessor style-field-values
                       :initarg :style-field-values
                       :type (vector string)
                       :documentation "Values of style fields in this entry"))
  (:documentation "A collection of fields used in a report row"))

(defmethod print-object ((obj report-entry) stream)
  (print-unreadable-object (obj stream)
    (format stream "RE: ")
    (loop for desc across (field-descriptors obj)
          for val  across (field-values obj)
          do (format stream "~a:~a" (id desc) val))))

(defclass report-row ()
  ((benchmark :accessor benchmark
              :initarg :benchmark
              :type string
              :documentation "Benchmark name for this row")
   (entries :accessor entries
            :initarg :entries
            :type (vector report-entry)
            :documentation "Entries in this report row"))
  (:documentation "A row of report entries"))

(defclass report-table ()
  ((title :accessor title
          :initarg :title
          :type string
          :documentation "The table's title")
   (solvers :accessor solvers
            :initarg :solvers
            :type vector ; TODO: more specific
            :documentation "Vector of solvers in this table")
   (styles :reader styles
           :initarg :styles
           :type list
           :documentation "List of report style configuration objects")
   (field-descriptors :accessor field-descriptors
                      :initarg :fields
                      :type (vector field-descriptor)
                      :documentation "Vector of field descriptors in each entry")
   (rows :accessor rows
         :initarg :rows
         :type (vector table-row)
         :documentation "Vector of table rows"))
  (:documentation "A report table"))

(defun get-report-field-descriptor (id)
  "Converts a field string to a field descriptor"
  (declare (type string id))
  (let ((opts nil))
    (flet ((mrfd (id &optional format name transformer)
             (let* ((location (str:downcase (symbol-name id)))
                    (format (or format "~a"))
                    (name (or name (str:title-case (str:replace-all "-" " " location))))
                    (transformer (or transformer #'identity)))
               (make-report-field-descriptor id :name name
                                                :location location
                                                :format format
                                                :transformer transformer
                                                :options opts)))
           (coerce-to-list (seq)
             "Coerces SEQ to a list"
             (coerce seq 'list)))
      (let ((pieces (str:split #\; id)))
        (loop for piece in (cdr pieces)
              for (key value) = (str:split #\= piece :limit 2)
              do (push (cons (str:downcase key) value) opts))
        (let ((name (cdr (assoc "name" opts :test #'string=))))
          (str:string-case (str:downcase (str:replace-all " " "-" (car pieces)))
            ("status" (mrfd :status nil name))
            ("name" (mrfd :name nil name))
            ("solver" (mrfd :solver nil name))
            ("run-time" (mrfd :run-time "~5f" name))
            ("time" (mrfd :run-time "~5f" name))
            ("load-time" (mrfd :load-time "~5f" name))
            ("gc-run-time" (mrfd :gc-run-time "~5f" name))
            ("peak-memory" (mrfd :peak-memory nil name))
            ("memory" (mrfd :peak-memory nil name))
            ("program" (mrfd :program nil name))
            ("verify-rate" (mrfd :verify-rate nil name))
            ("concrete-candidate-counter" (mrfd :concrete-candidate-counter nil name))
            ("concrete-counter" (mrfd :concrete-candidate-counter nil name))
            ("concrete" (mrfd :concrete-candidate-counter nil name))
            ("partial-candidate-counter" (mrfd :partial-candidate-counter nil name))
            ("partial-counter" (mrfd :partial-candidate-counter nil name))
            ("partial" (mrfd :partial-candidate-counter nil name))
            ("rate" (mrfd :verify-rate "~5f" name))
            ("specification-types" (mrfd :specification-types nil name))
            ("summary" (mrfd :summary nil name))
            ("prune-candidate-counter" (mrfd :prune-candidate-counter nil name))
            ("prune-attempt-counter" (mrfd :prune-attempt-counter nil name))
            ("prune-success-counter" (mrfd :prune-success-counter nil name))
            ("prune-success-rate" (mrfd :prune-success-rate "~,2f" name))
            ("prune-success" (mrfd :prune-success-rate "~,2f" name))
            ("full-verifier-time" (mrfd :full-verifier-time "~,2f" name))
            ("full-verifier-count" (mrfd :full-verifier-count nil name))
            ("quick-verifier-count" (mrfd :quick-verifier-count nil name))
            ("check-program-time" (mrfd :check-program-time "~,2f" name))
            ("cegis-time" (mrfd :cegis-time "~,2f" name))
            ("cegis-count" (mrfd :cegis-count nil name))
            ("concrete-candidates-by-size" (mrfd :concrete-candidates-by-size
                                                 "~{[~a]: ~a~^, ~}" name
                                                 #'coerce-to-list))
            ("by-size" (mrfd :concrete-candidates-by-size
                             "~{[~a]: ~a~^, ~}" name
                             #'coerce-to-list))
            ("max-size" (mrfd :concrete-candidates-by-size
                              nil name #'(lambda (x)
                                           (if (zerop (length x))
                                               nil
                                               (elt x 0)))))
            ("checkpoint-times" (mrfd :checkpoint-times
                                      "~{[~a]: ~,1f~^, ~}" name #'coerce-to-list))
            (otherwise (error "Unknown field: ~a" id))))))))

(defun get-report-field (field result-entry summary-entry)
  "Gets a report field from entries"
  (format nil
          (field-format field)
          (let ((val (funcall (field-transformer field)
                              (if (string= "summary" (location field))
                                  summary-entry
                                  (%ih result-entry (location field))))))
            (if (and (string= "verify-rate" (location field))
                     (null val))
                (let ((time (%ih result-entry "run-time"))
                      (cand (%ih result-entry "concrete-candidate-counter")))
                  (when (and time cand)
                    (/ cand time)))
                val))))

(defclass reporter ()
  ((styles :initarg :styles
           :reader styles
           :documentation "Report style strings"))
  (:documentation "A generic report writer"))

(defclass report-style ()
  ((name :reader style-name
         :initarg :name
         :type string
         :documentation "The name of the report style")
   (options :reader style-options
            :initarg :options
            :type list ; alist
            :documentation "Alist of style options"))
  (:documentation "A report style"))

(defun get-report-style-option (styles name &optional option)
  "Gets a report style option from STYLES, either a list of styles, table, or reporter"
  (setf styles (etypecase styles
                 (list styles)
                 (report-table (styles styles))
                 (reporter (styles styles))))
  (let ((style (find name styles :test #'string-equal :key #'style-name)))
    (when style
      (if (null option)
          style
          (cdr (assoc option (style-options style) :test #'string-equal))))))

(defclass report-style-provider () ()
  (:documentation "An extendable provider for report styles"))

(defgeneric provide-report-style (provider name options)
  (:documentation "Provides a report style. Implementors can return either:
* NULL, to signify that the provider doesn't support the style
* a symbol naming a REPORT-STYLE subclass, which will be instantiated
* an instance of REPORT-STYLE or subclass, which will be used as-is.")
  (:method-combination or)
  (:method :around (provider name options)
    "Transforms the output of the primary method into an actual style instance"
    (let ((style (call-next-method)))
      (cond
        ((null style) (error "No applicable style for name: ~a" name))
        ((symbolp style) (make-instance style :name name :options options))
        ((typep style 'report-style) style)
        (t (error "Invalid report style response: ~a" style)))))
  (:method or (provider name options)
    "Default method that requests the default style object"
    (declare (ignore provider name options))
    'report-style))

(defun make-report-style (style-string)
  "Parses and creates a style object from STYLE-STRING"
  (multiple-value-bind (name options)
      (u:parse-option-string style-string)
    (provide-report-style (make-instance 'report-style-provider) name options)))

(defgeneric report-style-required-fields (style)
  (:documentation "Returns a list of fields required by STYLE")
  (:method (style) nil))

(defmethod parse-report-into-table (reporter data
                                    &optional (fields '("summary")) solvers styles)
  "Parses suite data in DATA (as a hash table) into a report object, with the specified
list of FIELDS. If FIELDS is NIL, only includes the summary field"
  (declare (ignore solvers))
  (let ((name (%ih data "suite" "name"))
        (fields (map 'vector #'get-report-field-descriptor fields))
        (solvers (%ih data "solvers"))
        (table (make-array (length (%ih data "problems"))))
        (styles (map 'list #'make-report-style styles)))
    (loop with style-fields = (coerce
                               (remove-duplicates
                                (reduce #'append
                                        (map 'list #'report-style-required-fields
                                             styles)))
                               'vector)
          for d-row across (%ih data "result-matrix")
          for s-row across (%ih data "summary-matrix")
          for benchmark across (%ih data "problems")
          for row = (make-array (length (%ih data "solvers")))
          for row-ix from 0
          doing
             (loop for solver across solvers
                   for d-entry across d-row
                   for s-entry across s-row
                   for entry = (make-array (length fields))
                   for style-entry = (make-array (length style-fields))
                   for entry-ix from 0
                   doing
                      (loop for field across fields
                            for ix from 0
                            doing
                               (setf (aref entry ix)
                                     (get-report-field field d-entry s-entry)))
                      (loop for field across style-fields
                            for ix from 0
                            doing
                               (setf (aref style-entry ix)
                                     (get-report-field field d-entry s-entry)))
                   do (setf (aref row entry-ix)
                            (make-instance 'report-entry
                                           :field-descriptors fields
                                           :field-values entry
                                           :style-field-descriptors style-fields
                                           :style-field-values style-entry
                                           :solver solver)))
          do (setf (aref table row-ix)
                   (make-instance 'report-row
                                  :entries row
                                  :benchmark benchmark)))

    (make-instance 'report-table
                   :title name
                   :solvers solvers
                   :fields fields
                   :rows table
                   :styles styles)))

;;;
;;; Implementations
;;;
(defun %ih (table &rest keys)
  "Does a weird case-insensitive hashtable get"
  (flet ((iter-1 (key table)
           (multiple-value-bind (value found?)
               (gethash key table)
             (if found?
                 value
                 (gethash (str:upcase key)
                          table)))))
    (loop with curr-table = table
          for key in keys
          unless (null curr-table) do
             (setf curr-table (iter-1 key curr-table))
          finally (return curr-table))))

(defgeneric reporter-want-pathname (reporter)
  (:documentation "Returns T if the reporter wants a pathname instead of a stream")
  (:method (reporter) nil))

(defun invoke-report (reporter json-files output-file fields solvers styles)
  "Processes result files into a combined report"
  (map-into json-files #'u:rationalize-namestring json-files)
  (let* ((reporter (make-instance reporter
                                  :styles (map 'list #'make-report-style styles)))
         (data (loop for file in json-files
                     collect (parse-report-into-table
                              reporter
                              (jzon:parse file)
                              fields
                              solvers
                              styles))))
    (if (reporter-want-pathname reporter)
        (write-report reporter (u:rationalize-namestring output-file) data)
        (if (null output-file)
            (write-report reporter *standard-output* data)
            (uiop:with-output-file (ss (u:rationalize-namestring output-file)
                                       :if-exists :supersede)
              (write-report reporter ss data))))))

(defgeneric write-report-header (reporter stream)
  (:documentation "Writes a report header with REPORTER to STREAM."))

(defgeneric write-report-footer (reporter stream)
  (:documentation "Writes a report footer with REPORTER to STREAM."))

(defgeneric write-report-table (reporter stream table)
  (:documentation "Writes a report on TABLE with REPORTER to STREAM."))

(defgeneric write-report-table-header (reporter stream table)
  (:documentation "Writes a report table header on TABLE with REPORTER to STREAM."))

(defgeneric write-report-table-body (reporter stream table)
  (:documentation "Writes a report table body on TABLE with REPORTER to STREAM."))

(defgeneric write-report-table-row (reporter stream row table)
  (:documentation "Writes a report ROW on TABLE with REPORTER to STREAM."))

(defgeneric write-report-table-entry (reporter stream entry row table)
  (:documentation "Writes a report ENTRY for ROW on TABLE with REPORTER to STREAM."))

(defgeneric write-report-table-field (reporter stream descriptor value entry row table)
  (:documentation "Writes a report VALUE with field DESCRIPTOR for ENTRY for ROW
on TABLE with REPORTER to STREAM."))

(defgeneric write-report-empty (reporter stream)
  (:documentation "Writes a message that there are no tables with REPORTER to STREAM."))

(defgeneric write-report (reporter stream tables)
  (:documentation "Writes a report on TABLES with REPORTER to STREAM."))

;;;
;;; Default methods
;;;
(defmethod write-report (reporter ss data)
  "Writes a report from DATA into stream SS."
  (write-report-header reporter ss)

  (when (null data)
    (write-report-empty reporter ss))

  (loop for table in data
        do (write-report-table reporter ss table))
  (write-report-footer reporter ss))

(defmethod write-report-table-row (reporter ss row table)
  (loop for entry across (entries row)
        do (write-report-table-entry reporter ss entry row table)))

(defmethod write-report-table-entry (reporter ss entry row table)
  (loop for val across (field-values entry)
        for desc across (field-descriptors entry)
        do (write-report-table-field reporter ss desc val entry row table)))

(defmethod write-report-table (reporter ss table)
  "Writes a report table"
  ;; Write the table header. We need one cell per field per solver
  (write-report-table-header reporter ss table)

  ;; Write the table body
  (write-report-table-body reporter ss table))

(defmethod write-report-table-body (reporter ss table)
  (loop for row across (rows table)
        do (write-report-table-row reporter ss row table)))

;;;
;;; The HTML reporter
;;;
(defclass html-reporter (reporter)
  ()
  (:documentation "A reporter that writes HTML"))

(defgeneric report-table-row-classes (reporter row table)
  (:documentation "Gets CSS classes to set on the report row")
  (:method-combination append)
  (:method append (reporter row table) nil))

(defmethod write-report-header ((reporter html-reporter) ss)
  (declare (ignore reporter ss)))

(defmethod write-report-header :around ((reporter html-reporter) ss)
  "Writes a report header"
  (fmt-tab ss 0 "<!DOCTYPE html>~%<html>~%")
  (fmt-tab ss 1 "<head>~%")
  (call-next-method)
  (fmt-tab ss 1 "</head>~%")
  (fmt-tab ss 1 "<body>~%")
  (fmt-tab ss 2 "<h1>Benchmark Results</h1>~%"))

(defmethod write-report-footer ((reporter html-reporter) ss)
  "Writes a report footer"
  (fmt-tab ss 2 "<footer><p><em>Generated: ~a</em></p></footer>~%"
          (local-time:format-timestring nil (local-time:now)
                                        :format local-time:+rfc-1123-format+))
  (fmt-tab ss 1 "</body>~%")
  (fmt-tab ss 0 "</html>~%"))

(defun fmt-tab (stream tab-stop format &rest args)
  "Formats with leading spaces"
  (format stream "~a" (make-string (* 2 tab-stop) :initial-element #\Space))
  (apply #'format stream format args))

(defmethod write-report-table :before ((reporter html-reporter) ss table)
  (fmt-tab ss 2 "<h2>~a</h2>~%" (title table))
  (fmt-tab ss 2 "<div>~%")
  (fmt-tab ss 3 "<table>~%"))

(defmethod write-report-table :after ((reporter html-reporter) ss table)
  (fmt-tab ss 3 "</table>~%")
  (fmt-tab ss 2 "</div>~%"))

(defmethod write-report-table-header ((reporter html-reporter) ss table)
  (fmt-tab ss 4 "<thead>~%")
  (fmt-tab ss 5 "<tr><th>Solvers</th>")
  (loop with field-count = (length (field-descriptors table))
        for solver across (solvers table)
        do (format ss "<th colspan=~a>~a</th>" field-count solver))
  (format ss "</tr>~%")
  (fmt-tab ss 5 "<tr><th>Benchmarks</th>")
  (loop for solver across (solvers table)
        do (loop for field across (field-descriptors table)
                 do (format ss "<th>~a</th>" (name field))))
  (format ss "</tr>~%")
  (fmt-tab ss 4 "</thead>~%"))

(defmethod write-report-table-body :around ((reporter html-reporter) ss table)
  (fmt-tab ss 4 "<tbody>~%")
  (call-next-method)
  (fmt-tab ss 4 "</tbody>~%"))

(defmethod write-report-table-row :around ((reporter html-reporter) ss row table)
  (fmt-tab ss 5 "<tr~@[ class='~{~a~^ ~}'~]>"
           (report-table-row-classes reporter row table))
  (call-next-method)
  (format ss "</tr>~%"))

(defmethod write-report-table-row :before ((reporter html-reporter) ss row table)
  (format ss "<td>~a</td>" (benchmark row)))

(defmethod write-report-table-field
    ((reporter html-reporter) ss descriptor value entry row table)
  (format ss "<td>~a</td>" value))

(defmethod write-report-empty ((report html-reporter) ss)
  "Writes a notice that there are no reports in the table."
  (fmt-tab ss 2 "<p>No benchmark suites found.</p>~%"))

;;;
;;; Text reporter
;;;
(defclass text-reporter (reporter)
  ((column-widths :accessor column-widths
                  :initarg :column-widths
                  :type (vector integer)
                  :documentation "How wide each report column needs to be")
   (current-column :accessor current-column
                   :initarg :current-column
                   :type integer
                   :documentation "Which column is currently being printed"))
  (:documentation "A reporter that just writes text"))

(defmethod write-report-header ((reporter text-reporter) stream)
  (format stream "Benchmark Results~%"))

(defmethod write-report-empty ((reporter text-reporter) stream)
  (format stream "(no suites found)~%"))

(defun column-field (row ix)
  "Gets the field for the index IX in ROW"
  (declare (type report-row row)
           (type integer ix))
  (let ((orig-ix ix))
    (loop for entry across (entries row)
          for values = (field-values entry)
          if (< ix (length values)) do
            (return-from column-field (aref values ix))
          else do
            (setf ix (- ix (length values)))
          end)
    (error "Invalid column index: ~a" orig-ix)))

(defmethod write-report-table :around ((reporter text-reporter) stream table)
  ;; Compute column widths
  (let ((col-widths (make-array (1+ (* (length (field-descriptors table))
                                       (length (solvers table))))
                                :element-type 'integer
                                :initial-element 0)))
    (loop for row across (rows table) do
      (loop for col-width across col-widths
            for col-ix from 0
            ;; Special case: element 0 is the benchmark name
            do
               (setf (aref col-widths col-ix)
                     (max col-width
                          (if (zerop col-ix)
                              (length (benchmark row))
                              (length (column-field row (1- col-ix))))))))
    (setf (column-widths reporter) col-widths)
    (call-next-method)))

(defmethod write-report-footer ((reporter text-reporter) stream)
  (declare (ignore reporter stream)))

(defmethod write-report-table-header ((reporter text-reporter) stream table)
  (format stream "Suite: ~a~%" (title table))
  (let ((bar-length (+ (reduce #'+ (column-widths reporter))
                       (* 3 (length (column-widths reporter))))))
    (format stream "~a " (str:fit (aref (column-widths reporter) 0)
                                 "Solvers"
                                 :pad-side :center))
    (loop with field-count = (length (field-descriptors table))
          with field-widths = (+ (reduce #'+ (column-widths reporter) :start 1)
                                 field-count)
          for solver across (solvers table)
          do (format stream "| ~a" (str:fit field-widths solver :pad-side :center)))
    (format stream "~&~a " (str:fit (aref (column-widths reporter) 0)
                                    "Benchmarks"
                                    :pad-side :center))
    (loop for solver across (solvers table)
          do (loop for field across (field-descriptors table)
                   for field-ix from 1
                   do (format stream "| ~a " (str:fit (aref (column-widths reporter)
                                                          field-ix)
                                                    (name field)
                                                    :pad-side :center))))
    (format stream "~&~a~%" (make-string bar-length :initial-element #\-)))
  nil)

(defmethod write-report-table-row :before ((reporter text-reporter) stream row table)
  (let ((benchmark (benchmark row)))
    (format stream
            "~a~a "
            benchmark
            (make-string (- (aref (column-widths reporter) 0) (length benchmark))
                         :initial-element #\Space)))
  (setf (current-column reporter) 0))

(defmethod write-report-table-row :after ((reporter text-reporter) stream row table)
  (terpri stream))

(defmethod write-report-table-field
    ((reporter text-reporter) stream descriptor value entry row table)
  (incf (current-column reporter))
  (let ((width (aref (column-widths reporter) (current-column reporter))))
    (format stream "| ~a~a "
            value
            (make-string (- width (length value)) :initial-element #\Space))))

;;;
;;; Cactus reporter
;;;
(defclass cactus-reporter (reporter)
  ()
  (:documentation "A reporter for creating cactus plots"))

(defmethod parse-report-into-table :around ((reporter cactus-reporter) data
                                            &optional fields solvers styles)
  (let ((need-time (get-report-style-option reporter "include" "time-cutoff"))
        (has-status (find "status" fields :test #'string=))
        (has-time (or (find "time" fields :test #'string=)
                      (find "run-time" fields :test #'string=))))
    (if (and has-status (or (not need-time) has-time))
        (call-next-method)
        (progn
          (unless has-status
            (push "status" fields))
          (unless (or (not need-time) has-time)
            (push "run-time" fields))
          (call-next-method reporter data fields solvers styles)))))

(defmethod reporter-want-pathname ((reporter cactus-reporter)) t)

(defmethod write-report ((reporter cactus-reporter) output-path data &aux tc)
  "Writes a cactus report"
  (when (null data)
    (error "Cannot write an empty cactus plot"))

  (setf tc (get-report-style-option reporter "include" "time-cutoff"))

  (flet ((get-table-field (table)
           (let ((fields (field-descriptors table)))
             (loop for field across fields
                   unless (eql :status (id field))
                     do (return-from get-table-field field)))))
    ;;
    ;; Checks: all tables have the same solvers and the same (one) field
    ;;         but note that we'll also have the status field, so two
    ;;         maybe 3 if we have time
    ;;
    (unless (= 2 (length (field-descriptors (first data))))
      (or tc (error "Cactus reports can only have one field")))
    (let ((solvers (sort (solvers (first data)) #'string<))
          (field (get-table-field (first data))))
      (loop for table in (rest data)
            unless (equalp solvers (sort (solvers table) #'string<))
              do (error "Solvers not the same in table")
            unless (and (= 2 (length (field-descriptors table)))
                        (report-field-descriptor-equal
                         field (get-table-field table)))
              do (or tc (error "All tables must have the same field")))
      ;;
      ;; Create a vector of vectors mapping solver -> benchmark values
      ;;
      (let* ((row-count (reduce #'+ data :key #'(lambda (x)
                                                  (length (rows x)))))
             (columns (map 'vector #'(lambda (x)
                                       (declare (ignore x))
                                       (make-array row-count :initial-element nil))
                           solvers))
             (row-ix 0))
        ;;
        ;; Collect all of the data into columns for each solver
        ;;   Note that we only consider solved benchmarks here!
        ;;
        (loop with include-all = (get-report-style-option reporter
                                                          "include" "all")
              with include-unsolved = (get-report-style-option reporter
                                                               "include" "unsolved")
              with time-cutoff = (get-report-style-option reporter
                                                          "include" "time-cutoff")
              with exclude-zero = (get-report-style-option reporter
                                                           "include" "no-zero")
              for table in data do
          (loop for row across (rows table) do
            (loop for entry across (entries row)
                  for solver-ix = (position (solver entry) solvers :test #'string=)
                  for status-ix = (position :status (field-descriptors entry)
                                            :key #'id)
                  for time-ix = (and tc (position :run-time (field-descriptors entry)
                                                  :key #'id))
                  for value-ix = (or (and (not tc)
                                          (- 1 status-ix)) ; status-ix either 0 or 1
                                     (and (= 3 (length (field-descriptors entry)))
                                          (position nil (field-descriptors entry)
                                                    :key #'id
                                                    :test
                                                    (lambda (x y)
                                                      (not (or (eql x :run-time)
                                                               (eql x :status)
                                                               (eql y :run-time)
                                                               (eql y :status))))))
                                     time-ix) ; time cutoff and time field
                  for solved = (string= "SOLVED" (elt (field-values entry) status-ix))
                  do
                     (when (and (or include-all
                                    (and include-unsolved (not solved))
                                    (and (not include-unsolved) solved))
                                (or (not time-cutoff)
                                    (let ((runtime
                                            (read-from-string
                                             (elt (field-values entry) time-ix))))
                                      (>= runtime 5)))
                                (or (not exclude-zero)
                                    (not (zerop
                                          (read-from-string (elt (field-values entry)
                                                                 value-ix))))))
                       (setf (aref (elt columns solver-ix) row-ix)
                             (read-from-string (elt (field-values entry) value-ix)))))
            (incf row-ix)))
        ;;
        ;; Create a virtual "best" solver column
        ;;
        (let ((virtual-best (make-array row-count :initial-element nil)))
          (flet ((null-min (&rest numbers)
                   (let ((min nil))
                     (loop for x in numbers
                           if (and (not (null x))
                                   (or (null min)
                                       (< x min)))
                             do (setf min x))
                     min)))
            (loop for ix from 0 below row-count
                  do
                     (setf (aref virtual-best ix)
                           (apply #'null-min (map 'list #'(lambda (x) (aref x ix))
                                                  columns)))))
          ;;
          ;; Each column is sorted independently to create the cactus plot
          ;;   Note that we force NILs to the end - they won't be in the plot
          ;;
          (flet ((cactus-sort (v)
                   (sort v #'(lambda (a b)
                               (cond ((null a) nil)
                                     ((null b) t)
                                     (t (< a b)))))))
            (map-into columns #'cactus-sort columns)
            (setf virtual-best (cactus-sort virtual-best)))
          ;;
          ;; gnuplot time!
          ;;
          (if (get-report-style-option reporter "opt" "data-only")
              (with-open-file (fs output-path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
                (format fs "Index  ")
                (loop for ix from 0 below (length solvers)
                      do (format fs "'~a'  " (elt solvers ix)))
                (format fs "'Virtual Best'")
                (fresh-line fs)

                (loop for row-ix from 0 below (length (elt columns 0))
                      do (format fs "~a  " row-ix)
                         (loop for col-ix from 0 below (length columns)
                               do (format fs "~:[nan~;~:*~a~]  "
                                          (elt (elt columns col-ix) row-ix)))
                         (format fs "~:[nan~;~:*~a~]" (elt virtual-best row-ix))
                         (fresh-line fs)))

              (let ((gp:*gnuplot-home* (namestring
                                        (u:locate-exe "gnuplot"
                                                      :hint-path #P"d:/bin/gnuplot/bin/")))
                    (scale (/ 1280 640)))
                (when (null gp:*gnuplot-home*)
                  (error "Unable to find gnuplot on the system!"))

                (gp:with-plots (s :debug nil)
                  (gp:gp :set :terminal '(pngcairo)
                              :size :|1280,960|
                         :fontscale scale
                         :linewidth scale
                         :pointscale scale)
                  (gp:gp :set :output output-path)
                  (gp:gp :set :xlabel "Benchmark Count")
                  (gp:gp :set :ylabel (name field))
                  (gp:gp :set :xrange (list 0 row-count))
                  (gp:gp :set :key :left :top)

                  (let ((title (get-report-style-option reporter "opt" "title")))
                    (when title (gp:gp :set :title title)))


                  (loop for ix from 0 below (length solvers)
                        for solver = (elt solvers ix)
                        for column = (elt columns ix)
                        unless (let ((exclude-opt
                                       (find "exclude" (styles reporter)
                                             :key #'style-name
                                             :test #'string-equal)))
                                 (and exclude-opt
                                      (cdr (assoc solver (style-options exclude-opt)
                                                  :test #'string-equal))))
                          do
                             (gp:plot #'(lambda ()
                                          (loop for val across column
                                                for x from 0
                                                until (null val)
                                                do (format s "~&~a ~f" x val)))
                                      :using '(1 2)
                                      :title solver
                                      :with '(:lines)))
                  (unless (find "no-virtual-best" (styles reporter)
                                :key #'style-name :test #'string-equal)
                    (gp:plot #'(lambda ()
                                 (loop for val across virtual-best
                                       for x from 0
                                       until (null val)
                                       do (format s "~&~a ~f" x val)))
                             :using '(1 2)
                             :title "Virtual Best"
                             :with '(:lines)))))))))))

;;;
;;; Comparison reporter
;;;
(defclass comparison-reporter (reporter)
  ()
  (:documentation "A reporter for comparing things on plots"))

(defmethod parse-report-into-table :around ((reporter comparison-reporter) data
                                            &optional fields solvers styles)
  (if (find "status" fields :test #'string=)
      (call-next-method)
      (call-next-method reporter data (cons "status" fields) solvers styles)))

(defmethod reporter-want-pathname ((reporter comparison-reporter)) t)

(defmethod write-report ((reporter comparison-reporter) output-path data)
  "Writes a cactus report"
  (when (null data)
    (error "Cannot write an empty comparison plot"))

  (flet ((get-table-field (table)
           (let ((fields (field-descriptors table)))
             (loop for field across fields
                   unless (eql :status (id field))
                     do (return-from get-table-field field)))))
    ;;
    ;; Checks: all tables have the same solvers and the same (one) field
    ;;         but note that we'll also have the status field, so two
    ;;
    (unless (= 2 (length (field-descriptors (first data))))
      (error "Comparison reports can only have one field"))
    (let ((solvers (solvers (first data)))
          (field (get-table-field (first data))))

      (let ((solvers-style (get-report-style-option reporter "solvers")))
        (when solvers-style
          (setf solvers
                (delete-if-not #'(lambda (s)
                                   (get-report-style-option reporter "solvers" s))
                               solvers))))

      (loop for table in (rest data)
            unless t; (equalp solvers (solvers table))
              do (error "Solvers not the same in table")
            unless (and (= 2 (length (field-descriptors table)))
                        (report-field-descriptor-equal
                         field (get-table-field table)))
              do (error "All tables must have the same field"))
      ;;
      ;; Create a vector of vectors mapping solver -> benchmark values
      ;;
      (let* ((row-count (reduce #'+ data :key #'(lambda (x)
                                                  (length (rows x)))))
             (columns (map 'vector #'(lambda (x)
                                       (declare (ignore x))
                                       (make-array row-count :initial-element nil))
                           solvers))
             (row-ix 0))
        ;;
        ;; Collect all of the data into columns for each solver
        ;;   Note that we only consider solved benchmarks here!
        ;;
        (loop with include-all = (get-report-style-option reporter
                                                          "include" "all")
              with include-unsolved = (get-report-style-option reporter
                                                               "include" "unsolved")
              for table in data do
          (loop for row across (rows table) do
            (loop for entry across (entries row)
                  for solver-ix = (position (solver entry) solvers :test #'string=)
                  for status-ix = (position :status (field-descriptors entry)
                                            :key #'id)
                  for value-ix = (- 1 status-ix) ;; status-ix either 0 or 1
                  for solved = (string= "SOLVED" (elt (field-values entry) status-ix))
                  unless (null solver-ix)
                  do
                     (when (or include-all
                               (and include-unsolved (not solved))
                               (and (not include-unsolved) solved))
                       (setf (aref (elt columns solver-ix) row-ix)
                             (read-from-string (elt (field-values entry) value-ix)))))
            (incf row-ix)))
        ;;
        ;; Create a virtual "best" solver column
        ;;
        (let ((virtual-best (make-array row-count :initial-element nil)))
          (flet ((null-min (&rest numbers)
                   (let ((min nil))
                     (loop for x in numbers
                           if (and (not (null x))
                                   (or (null min)
                                       (< x min)))
                             do (setf min x))
                     min)))
            (loop for ix from 0 below row-count
                  do
                     (setf (aref virtual-best ix)
                           (apply #'null-min (map 'list #'(lambda (x) (aref x ix))
                                                  columns)))))
          ;;
          ;; Process and sort
          ;;
          (flet ((cactus-sort (v &key key)
                   (sort v #'(lambda (a b)
                               (cond ((null a) nil)
                                     ((null b) t)
                                     (t (< a b))))
                         :key key)))
            (let ((combined (apply #'map 'list #'list (coerce columns 'list))))
              (setf combined (cactus-sort combined :key #'first))
              (loop for x in combined
                    for ix from 0
                    do (loop for col from 0 below (length columns)
                             do (setf (aref (aref columns col) ix)
                                      (nth col x)))))

            ;;
            ;; Percent difference?
            ;;
            (let ((%diff-base (get-report-style-option reporter "opt" "%diff")))
              (assert (= 2 (length columns)))
              (when %diff-base
                (let* ((sv-1 (or (and %diff-base
                                      (position %diff-base solvers
                                                :test #'string-equal))
                                 0))
                       (sv-2 (- 1 sv-1)))
                  (setf columns
                        (list
                         (cactus-sort
                          (map 'vector #'(lambda (x y)
                                           (unless (or (null x)
                                                       (null y))
                                             (* 100
                                                (/ (- x y)
                                                   (/ (+ x y) 2)))))
                               (elt columns sv-1)
                               (elt columns sv-2)))))))))

          (if (get-report-style-option reporter "opt" "data-only")
              (with-open-file (fs output-path
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
                (format fs "Index  ")
                (loop for ix from 0 below (length solvers)
                      do (format fs "'~a'  " (elt solvers ix)))
                ;;(format fs "'Virtual Best'")
                (fresh-line fs)

                (loop for row-ix from 0 below (length (elt columns 0))
                      do (format fs "~a  " row-ix)
                         (loop for col-ix from 0 below (length columns)
                               do (format fs "~:[nan~;~:*~a~]  "
                                          (elt (elt columns col-ix) row-ix)))
                         ;;(format fs "~:[nan~;~:*~a~]" (elt virtual-best row-ix))
                         (fresh-line fs)))

              ;;
              ;;
              ;; gnuplot time!
              ;;
              (let ((gp:*gnuplot-home* (namestring
                                        (u:locate-exe "gnuplot"
                                                      :hint-path #P"d:/bin/gnuplot/bin/")))
                    (scale (/ 1280 640))
                    (x-length row-count))
                (when (null gp:*gnuplot-home*)
                  (error "Unable to find gnuplot on the system!"))

                (when (get-report-style-option reporter "opt" "shrink-x")
                  (let ((vb-end (position nil virtual-best))
                        (others (map 'list
                                     #'(lambda (r)
                                         (position nil r))
                                     columns)))
                    (setf x-length (apply #'max vb-end others))))

                (gp:with-plots (s :debug (get-report-style-option reporter "opt" "debug"))
                  (gp:gp :set :terminal '(pngcairo)
                              :size :|1280,960|
                         :fontscale scale
                         :linewidth scale
                         :pointscale scale)
                  (gp:gp :set :output output-path)
                  (gp:gp :set :xlabel "Benchmark Count")
                  (gp:gp :set :ylabel (or (get-report-style-option reporter
                                                                   "opt" "ylabel")
                                          (format nil "% difference in ~a"
                                                  (name field))))
                  (gp:gp :set :xrange (list 0 x-length))
                  (gp:gp :set :key :left :top)

                  (let ((title (get-report-style-option reporter "opt" "title")))
                    (when title (gp:gp :set :title title)))


                  (when (get-report-style-option reporter "opt" "zero-line")
                    (gp:gp :set :xzeroaxis))

                  (if (get-report-style-option reporter "opt" "%diff")
                      (gp:plot #'(lambda ()
                                   (loop for val across (first columns)
                                         for x from 0
                                         until (null val)
                                         do (format s "~&~a ~f" x val)))
                               :using '(1 2)
                               :title (format nil "~a vs ~a"
                                              (elt solvers 0) (elt solvers 1))
                               :with '(:lines))

                      (loop for ix from 0 below (length solvers)
                            for solver = (elt solvers ix)
                            for column = (elt columns ix)
                            unless (let ((exclude-opt
                                           (find "exclude" (styles reporter)
                                                 :key #'style-name
                                                 :test #'string-equal)))
                                     (and exclude-opt
                                          (cdr (assoc solver (style-options exclude-opt)
                                                      :test #'string-equal))))
                              do
                                 (gp:plot #'(lambda ()
                                              (loop for val across column
                                                    for x from 0
                                                    until (null val)
                                                    do (format s "~&~a ~f" x val)))
                                          :using '(1 2)
                                          :title solver
                                          :with '(:lines))))
                  (unless (find "no-virtual-best" (styles reporter)
                                :key #'style-name :test #'string-equal)
                    (gp:plot #'(lambda ()
                                 (loop for val across virtual-best
                                       for x from 0
                                       until (null val)
                                       do (format s "~&~a ~f" x val)))
                             :using '(1 2)
                             :title "Virtual Best"
                             :with '(:lines)))))))))))

;;;
;;; Delimited text reporter (CSV, TSV, etc.)
;;;
(defclass delimited-text-reporter (reporter)
  ()
  (:documentation "A reporter for exporting data as delimited text"))

(defmethod write-report-header ((reporter delimited-text-reporter) stream) t)
(defmethod write-report-footer ((reporter delimited-text-reporter) stream) t)
(defmethod write-report-table-header ((reporter delimited-text-reporter) stream table)
  nil)

(defmethod write-report-table-row ((reporter delimited-text-reporter)
                                   ss row table)
  "Writes a CSV row"
  (let ((row-data (cons (benchmark row) nil)))
    (loop for entry across (entries row)
          do (loop for value across (field-values entry)
                   do (push value row-data)))
    (cl-csv:write-csv-row (nreverse row-data) :stream ss)))
