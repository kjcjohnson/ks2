;;;;
;;;; KS3 runner
;;;;
(in-package #:com.kjcjohnson.ks3.runner)

(defun spawn-inferior-lisp (port-file)
  "Spawns an inferior lisp. Currently, just SBCL."
  (uiop:launch-program (list "sbcl"
                             "--disable-ldb"
                             "--lose-on-corruption"
                             "--dynamic-space-size"
                             "2048" ; MiB
                             "--end-runtime-options"

                             "--eval"
                             "(ql:quickload \"com.kjcjohnson.ks3.runner/helper\")"
                             "--eval"
                             (format nil
                                     "(com.kjcjohnson.ks3.runner.helper:init-and-start-swank ~s)"
                                     port-file)
                             
                             #|
                             "--eval"
                             "(format t \"STARTUP~%\")"
                             "--eval"
                             "(ql:quickload \"swank\")"
                             "--eval"
                             "(format t \"SWANK LOADED~%\")"
                             "--eval"
                             "(setf swank:*configure-emacs-indentation* nil)"
                             "--eval"
                             "(format t \"SWANK CONFIGURED~%\")"
                             "--eval"
                             (format nil
                                     "(swank:start-server ~s :dont-close t)"
                                     portfile)
                             "--eval"
                             "(format t \"SWANK STARTED~%\")"
                             "--eval"
                             "(loop (sleep 60))"
                             |#
                             "--end-toplevel-options")
                       :output :interactive))

(defun swank-spawn ()
  "Spawns an inferior lisp with a SWANK connection"
  (let ((portfile (uiop:tmpize-pathname
                   (merge-pathnames "portfile"
                                    (uiop:temporary-directory)))))
    (delete-file portfile)
    (unwind-protect
         (let ((il-conn (spawn-inferior-lisp portfile)))
           (loop
             for i from 0 to 30
             doing (sleep 1)
             until (uiop:file-exists-p portfile))
           (let* ((port (with-open-file (pfs portfile :direction :input)
                          (read pfs)))
                  (swank-conn (swank-protocol:make-connection
                               "localhost"
                               port
                               :logp nil)))
             (swank-protocol:connect swank-conn)
             (let ((spawn (make-instance 'child-lisp
                                         :process il-conn
                                         :port port
                                         :swank-connection swank-conn)))
               (init-swank-response-loop spawn)
               spawn)))
      (delete-file portfile))))
