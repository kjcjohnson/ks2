;;;;
;;;; Bootstrap-helper.lisp - bootstraps the TDP systems
;;;;
(require 'asdf)

(defun build-runner ()
  "Builds the runner image"
  (proclaim '(optimize (speed 3) (debug 0)))
  (asdf:oos 'asdf:load-op "com.kjcjohnson.tdp/test" :force t)
  (trivial-garbage:gc :full t)
  (asdf:oos 'asdf:image-op "com.kjcjohnson.ks2.runner/helper"))

(defun build-app ()
  "Builds the controller app"
  (asdf:oos 'asdf:program-op "com.kjcjohnson.ks2.runner"))
