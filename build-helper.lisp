;;;;
;;;; Bootstrap-helper.lisp - bootstraps the TDP systems
;;;;
(proclaim '(optimize (speed 3) (debug 0)))
(asdf:oos 'asdf:load-op "com.kjcjohnson.tdp/test" :force t)
(setf com.kjcjohnson.synthkit.ast:*execution-counter* 0)
(trivial-garbage:gc :full t)
(asdf:oos 'asdf:image-op "com.kjcjohnson.ks2.runner/helper")
