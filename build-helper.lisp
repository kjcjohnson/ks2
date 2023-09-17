;;;;
;;;; Bootstrap-helper.lisp - bootstraps the TDP systems
;;;;
(require 'asdf)

(defun build-core (&key disable-smt optimize)
  "Builds the core image"
  (when disable-smt (push :synthkit-disable-smt-solver *features*))
  (ql:quickload "com.kjcjohnson.tdp/test")
  (proclaim `(optimize
              ,@(case optimize
                 (:speed '((speed 3) (debug 0)))
                 (:debug '((speed 1) (debug 3)))
                 (otherwise '((speed 1) (debug 2))))))
  (when optimize
    (asdf:oos 'asdf:compile-op "com.kjcjohnson.synthkit" :force t)
    (asdf:oos 'asdf:load-op "com.kjcjohnson.tdp/test" :force t))
  (asdf:oos 'asdf:image-op "com.kjcjohnson.ks2.runner/helper"))

(defun build-app (&key public-release)
  "Builds the controller app"
  (when public-release (push :ks2-public-release *features*))
  (ql:quickload "com.kjcjohnson.ks2.runner")
  (ql:quickload "com.kjcjohnson.ks2/app")
  (asdf:oos 'asdf:program-op "com.kjcjohnson.ks2/app-all"))

(defun build-docs (filename)
  "Builds the command documentation"
  (asdf:oos 'asdf:load-op "com.kjcjohnson.ks2/app-all")
  (funcall (uiop:ensure-function "generate-documentation"
                                 :package '#:com.kjcjohnson.ks2.cli)
           filename))
