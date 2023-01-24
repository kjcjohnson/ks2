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
                    (#:enum #:com.kjcjohnson.tdp.enumerative)
                    (#:duet #:com.kjcjohnson.tdp.duet)
                    (#:frangel #:com.kjcjohnson.frangel)
                    (#:tde #:com.kjcjohnson.tdp.top-down-enum)
                    (#:tdp-test #:com.kjcjohnson.tdp.test))
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

(defslimefun enum-solve (problem-file &key max-depth)
  (let ((problem (maybe-load-problem-file problem-file)))
    (format nil "~s"
            (enum::enum-solve problem
                              :max-depth (if (null max-depth)
                                             8
                                             max-depth)))))

(defslimefun duet-solve (problem-file &key depth)
  (let ((problem (maybe-load-problem-file problem-file)))
    (format nil "~s"
            (duet::duet-solve problem :depth (if (null depth)
                                                 8
                                                 depth)))))

(defslimefun frangel-solve (problem-file)
  (let ((problem (maybe-load-problem-file problem-file)))
    (format nil "~s"
            (frangel::fragment-search problem))))

(defslimefun tde-solve (problem-file)
  (let ((problem (maybe-load-problem-file problem-file)))
    (format nil "~s"
            (tde::top-down-enum-solve problem))))

(defslimefun load-problem-file (problem-file)
  (setf *problem-file* (semgus:load-semgus-problem problem-file))
  t)
