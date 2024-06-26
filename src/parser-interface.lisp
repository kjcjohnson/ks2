;;;;
;;;; Parser interface - connects to the SemGuS Parser
;;;;
(in-package #:com.kjcjohnson.ks2)

(defparameter *s-expression-file-extensions*
  '("sexp" "sexpr")
  "File extensions that we assume are S-expression benchmark files")

(defparameter *semgus-file-extensions*
  '("sem" "sl" "smt")
  "File extensions that we assume are SemGuS benchmark files (in SMT-LIB2 format)")

(defun parse-benchmark-file (pathname)
  "Parses a benchmark file"
  (let ((output-pathname (make-pathname :defaults pathname :type "sexpr")))
    (unless (and (uiop:file-exists-p output-pathname)
                 (> (file-write-date output-pathname)
                    (file-write-date pathname)))

      (let ((parser-exe (u:locate-exe "semgus-parser")))
        (when (null parser-exe)
          (error "Cannot find `semgus-parser` executable. Ensure it is in the path."))
        (uiop:run-program (list (namestring parser-exe)
                                "--format" "sexpr"
                                "--output" (namestring output-pathname)
                                (namestring pathname))
                          :output t
                          :error-output t)))
    output-pathname))
  
(defun ensure-sexp-benchmark-file (pathname)
  "Ensures that PATHNAME designates a benchmark file in S-expression format. If not,
runs the SemGuS Parser (if available). Returns the (maybe) new pathname."

  ;; We always want an actual pathname object
  (handler-case
      (setf pathname (uiop:ensure-pathname pathname
                                           :ensure-absolute t
                                           :defaults (uiop:getcwd)))
    (parse-error ()
      (error (str:concat "Invalid benchmark filename: " pathname))))

  (let ((ext (pathname-type pathname)))
    (cond
      ((not (uiop:file-exists-p pathname))
       (error (str:concat "Benchmark file does not exist: " (namestring pathname))))
      
      ((find ext *s-expression-file-extensions* :test #'string-equal)
       pathname)
      
      ((find ext *semgus-file-extensions* :test #'string-equal)
       (parse-benchmark-file pathname))
      
      (t
       ;; Assume the user is right?
       pathname))))
