;;;;
;;;; Report functionality
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

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
          
    
(defun invoke-report (json-files output-file)
  "Processes result files into a combined report"
  (map-into json-files #'u:rationalize-namestring json-files)

  (let ((data (loop for file in json-files
                    collect (jzon:parse file))))
    (if (null output-file)
        (write-report *standard-output* data)
        (uiop:with-output-file (ss (u:rationalize-namestring output-file)
                                   :if-exists :supersede)
          (write-report ss data)))))

(defun write-report (ss data)
  "Writes a report from DATA into stream SS."
  (format ss "<!DOCTYPE html>
<html>
  <body>
    <h1>Benchmark Results</h1>
")

  (when (null data)
    (format ss "    <p>No benchmark suites found.</p>~%"))
  
  (loop for table in data
        do
           (format ss "    <h2>~a</h2>~%" (%ih table "suite" "name"))
           (format ss "    <div>~%")
           (format ss "      <table>~%")
           ;; header
           (format ss
                   "        <thead><tr><th>Benchmark</th>~{<th>~a</th>~}</tr></thead>~%"
                   (coerce (%ih table "solvers") 'list))
           (loop
             with sum-mat = (%ih table "summary-matrix")
             for problem across (%ih table "problems")
             for sub-mat across sum-mat
             do
                (format ss "        <tr><td>~a</td>~{<td>~a</td>~}</tr>~%"
                        problem
                        (loop for res across sub-mat collecting res)))
           
           (format ss "      </table>~%")
           (format ss "    </div>~%"))
  (format ss "    <footer><p><em>Generated: ~a</em></p></footer>~%"
          (local-time:format-timestring nil (local-time:now)
                                        :format local-time:+rfc-1123-format+))
  (format ss "  </body>
</html>~%"))
