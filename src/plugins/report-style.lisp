;;;;
;;;; Report styling package
;;;;
(defpackage #:com.kjcjohnson.ks2.plugins.report-style
  (:use #:cl)
  (:local-nicknames (#:plug #:systems.duck.plugboard)
                    (#:cli #:com.kjcjohnson.ks2.cli)))

(in-package #:com.kjcjohnson.ks2.plugins.report-style)

(plug:defplugin report-style ()
  ()
  (:documentation "A report styling plugin")
  (:extends (cli::html-reporter cli::report-style-provider)))

(defmethod cli::write-report-header :after ((reporter report-style) ss)
  (cli::fmt-tab ss 2 "<style>
body {
  margin:40px auto;
  max-width: 650px;
  line-height: 1.6;
  font-size: 18px;
  color: #444;
  padding:0 10px;
}
h1, h2, h3 {
  line-height: 1.2;
}
table {
  width: 100%;
}
table, th, td {
  border-collapse: collapse;
  border: solid black 1px;
}
th, td {
  padding-left: 1em;
  padding-right: 1em;
}
th {
  background: antiquewhite;
}
tr > td:first-child {
  background: floralwhite;
}
tr.highlight.highlight-better {
  background: darkseagreen;
}
tr.highlight.highlight-worse {
  background: indianred;
}
tr.highlight.highlight-equiv {
  background: lightgoldenrodyellow;
}
    </style>~%"))

(defclass highlight-report-style (cli::report-style)
  ((base :reader highlight-base
         :initarg :base
         :type string
         :documentation "The name of the base solver to compare against")
   (compare :reader highlight-compare
            :initarg :compare
            :type string
            :documentation "The name of the comparison solver to compare against")
   (field :reader highlight-field
          :initarg :field
          :type string
          :documentation "The name of the field to compare with"))
  (:documentation "A style for highlighting table rows based on comparisons"))

(defmethod cli::report-style-required-fields ((style highlight-report-style))
  (list (highlight-field style)))

(defmethod cli::provide-report-style or ((provider report-style) name options)
  ;; Highlight style
  (when (string-equal name "highlight")
    (let ((base (cdr (assoc "base" options :test #'string-equal)))
          (compare (cdr (assoc "compare" options :test #'string-equal)))
          (field (cdr (assoc "field" options :test #'string-equal))))

      (when (null base) (error "Highlight option missing 'base' property"))
      (when (null compare) (error "Highlight option missing 'compare' property"))
      (when (null field) (error "Highlight option missing 'field' property"))

      (make-instance 'highlight-report-style
                     :name name
                     :options options
                     :base base
                     :compare compare
                     :field (cli::get-report-field-descriptor field)))))

(defmethod cli::report-table-row-classes append ((reporter report-style) row table)
  (let ((highlight (find (find-class 'highlight-report-style)
                         (cli::styles table)
                         :key #'class-of)))
    (when highlight
      (let ((base nil)
            (compare nil))
        (loop for entry across (cli::entries row)
              when (string-equal (cli::solver entry) (highlight-base highlight))
                do (setf base entry)
              when (string-equal (cli::solver entry) (highlight-compare highlight))
                do (setf compare entry))

        (when (and base compare)
          (let* ((field-ix (position (highlight-field highlight)
                                     (cli::style-field-descriptors base)))
                 (base-val (read-from-string (elt (cli::style-field-values base)
                                                  field-ix)))
                 (compare-val (read-from-string (elt (cli::style-field-values compare)
                                                     field-ix)))
                 (diff (or (and (zerop base-val) 1)
                           (/ (abs (- base-val compare-val)) base-val))))
            (list "highlight"
                  (cond
                    ((<= diff 0.1)
                     "highlight-equiv")
                    ((< compare-val base-val)
                     "highlight-better")
                    (t
                     "highlight-worse")))))))))
