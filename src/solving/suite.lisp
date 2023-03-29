;;;;
;;;; Suites
;;;;
(in-package #:com.kjcjohnson.ks2.solving)

(defclass suite ()
  ((name :reader name
         :initarg :name
         :type string
         :documentation "The friendly name of this suite")
   (problems :reader problems
             :initarg :problems
             :type (vector problem)
             :documentation "Vector of problems in this suite"))
  (:documentation "A suite of benchmark problems"))

(defparameter *suite-file-type-preference*
  (vector "sem" "sl" "smt" "sexpr" "sexp")
  "List of file types, in preference order, for resolving a single benchmark file")

(defun %suite-preferred-file (file1 file2)
  "Compares two file pathnames, returning the preferred pathname"
  (declare (type (or pathname null) file1 file2))
  (flet ((get-score (pathname)
           "Gets a preference score for a pathname. Smaller is better."
           (and pathname
                (position (pathname-type pathname)
                          *suite-file-type-preference*
                          :test #'string=))))
    (let ((score1 (get-score file1))
          (score2 (get-score file2)))
      (cond
        ((and (null score1) (null score2)) nil)
        ((null score1) file2)
        ((null score2) file1)
        ((< score1 score2) file1)
        (t file2)))))

(defun make-suite-from-directory (pathname &key transformer)
  "Makes a suite from a directory of benchmark files"
  (let ((benchmark-dir (uiop:ensure-directory-pathname pathname)))
    (unless (uiop:directory-exists-p benchmark-dir)
      (v:error :ks2.solving.suite "Missing suite directory: ~a (originally ~a)"
               benchmark-dir pathname)
      (error "Suite directory does not exist: ~a" benchmark-dir))

    ;; We have to merge files with the same name into one benchmark entry
    ;; E.g., test.sem and test.sexpr need to be just one entry.
    (let* ((benchmark-files (uiop:directory-files benchmark-dir))
           (name-table (make-hash-table :test #'equal :size (length benchmark-files))))
      (dolist (file benchmark-files)
        (v:trace :ks2.solving.suite "Discover file: ~a" file)
        (let* ((previous (gethash (pathname-name file) name-table))
               (kept (%suite-preferred-file previous file)))
          (v:trace :ks2.solving.suite "Kept file: ~a" kept)
          (setf (gethash (pathname-name file) name-table) kept)))

      (let ((problems (loop for file being the hash-values of name-table
                            unless (null file) do
                              (v:debug :ks2.solving.suite "Using benchmark: ~a" file)
                            and
                              collect (make-problem file :transformer transformer))))
        (make-instance 'suite
                       ;; Yikes. We need a more rigorous way of getting the suite name
                       :name (string (car (last (pathname-directory benchmark-dir))))
                       :problems problems)))))
