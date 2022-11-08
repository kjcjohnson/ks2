;;;;
;;;; Bootstrap-helper.lisp - bootstraps the TDP systems
;;;;
(require 'asdf)

(defun build-runner (&key disable-smt)
  "Builds the runner image"
  (when disable-smt (push :synthkit-disable-smt-solver *features*))
  (ql:quickload "com.kjcjohnson.tdp/test")
  (proclaim '(optimize (speed 3) (debug 0)))
  (asdf:oos 'asdf:compile-op "com.kjcjohnson.synthkit" :force t)
  (asdf:oos 'asdf:load-op "com.kjcjohnson.tdp/test" :force t)
  (asdf:oos 'asdf:image-op "com.kjcjohnson.ks2.runner/helper"))

(defun build-app ()
  "Builds the controller app"
  (ql:quickload "com.kjcjohnson.ks2.runner")
  (asdf:oos 'asdf:program-op "com.kjcjohnson.ks2.runner"))
