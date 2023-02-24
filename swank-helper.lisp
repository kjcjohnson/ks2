;;;;
;;;; Swank helper for the ks2 runner
;;;;
(eval-when (:compile-toplevel :load-toplevel)
  (when (find-package '#:com.kjcjohnson.ks2.runner.helper)
    (delete-package '#:com.kjcjohnson.ks2.runner.helper)))

(defpackage #:com.kjcjohnson.ks2.runner.helper
  (:use :cl)
  (:local-nicknames (#:semgus #:com.kjcjohnson.synthkit.semgus)
                    (#:ast #:com.kjcjohnson.synthkit.ast)
                    (#:solver-api #:com.kjcjohnson.ks2.solver-api))
  (:export #:init-and-start-swank)
  (:import-from #:swank
                #:defslimefun))

(in-package #:com.kjcjohnson.ks2.runner.helper)

(defun init-and-start-swank (port-file)
  "Initialize and start a Swank server. Write the port to portfile."
  (when (uiop:file-exists-p port-file)
    (delete-file port-file))

  (setf swank:*configure-emacs-indentation* nil)

  (swank:start-server port-file :dont-close t)

  (loop (sleep 60)))

(defslimefun get-used-dynamic-space ()
  (sb-kernel:dynamic-usage))

(defslimefun get-total-dynamic-space ()
  (sb-ext:dynamic-space-size))

(defslimefun get-execution-counter ()
  ast:*execution-counter*)

(defslimefun force-gc ()
  (trivial-garbage:gc :full t))

(defslimefun get-gc-run-time ()
  #+sbcl sb-ext:*gc-run-time*
  #-sbcl nil)

(defslimefun get-statistics ()
  (list
   :dynamic-space-used (sb-kernel:dynamic-usage)
   :dynamic-space-total (sb-ext:dynamic-space-size)
   :gc-run-time sb-ext:*gc-run-time*
   :execution-counter ast:*execution-counter*
   :internal-real-time (get-internal-real-time)
   :internal-run-time (get-internal-run-time)))

(defslimefun bootstrap-tdp ()
  (unless (find :ks2-bootstrapped *features*)
    (proclaim '(optimize (speed 3) (debug 0)))
    (asdf:oos 'asdf:load-op "com.kjcjohnson.tdp/test" :force t)
    (push :ks2-bootstrapped *features*))
  (setf ast:*execution-counter* 0)
  (trivial-garbage:gc :full t)
  t)

(defvar *problem-file* nil "The current loaded problem file")

(defun maybe-load-problem-file (problem-file)
  "Loads a problem file if specified, or falls back to *PROBLEM-FILE*"
  (if problem-file
      (semgus:load-semgus-problem problem-file)
      *problem-file*))

(defslimefun load-problem-file (problem-file)
  (setf *problem-file* (semgus:load-semgus-problem problem-file))
  t)

;;;
;;; Solver API RPCs
;;;
(defslimefun list-solvers ()
  (solver-api:list-solvers))

(defslimefun solver-name (solver-designator)
  (solver-api:solver-name solver-designator))

(defslimefun solver-symbol (solver-designator)
  (solver-api:solver-symbol solver-designator))

(defslimefun solver-description (solver-designator)
  (solver-api:solver-description solver-designator))

(defslimefun solver-action (solver-designator)
  (solver-api:solver-action solver-designator))

(defslimefun solver-options (solver-designator)
  (solver-api:solver-options solver-designator))

(defun actually-print-program-node (pn)
  "Prints a decent representation of a program node PN."
  (if (typep pn 'ast:program-atom)
      (let ((ast::*printing-program-node* t))
        (with-output-to-string (string-stream)
          (ast::print-program-node pn string-stream)))
      (format nil "~s" pn)))

(defslimefun solve-problem (solver-designator problem-file
                                              &rest options &key &allow-other-keys)
  (let* ((problem (maybe-load-problem-file problem-file))
         (results (apply #'solver-api:solve-problem solver-designator problem options)))
    ;; TODO: create and serialize results into a proxy object
    (cond
      ((and (listp results) (= 1 (length results)))
       (actually-print-program-node (first results)))
      ((typep results 'sequence)
       (format nil "~s"
               (map 'list #'actually-print-program-node results)))
      (t
       (actually-print-program-node results)))))
