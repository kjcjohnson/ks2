;;;;
;;;; KS2 runner
;;;;
(in-package #:com.kjcjohnson.ks2.runner)

(defun spawn-inferior-lisp (port-file &key image)
  "Spawns an inferior lisp. Currently, just SBCL."
  (uiop:launch-program (list (if (null image)
                                 "sbcl"
                                 image)
                             "--disable-ldb"
                             "--lose-on-corruption"
                             "--dynamic-space-size"
                             "2048" ; MiB
                             "--end-runtime-options"

                             "--eval"
                             (if (null image)
                                 "(ql:quickload \"com.kjcjohnson.ks2.runner/helper\")"
                                 "(push :ks2-bootstrapped *features*)")
                             "--eval"
                             (format nil
                                     "(com.kjcjohnson.ks2.runner.helper:init-and-start-swank ~s)"
                                     port-file)

                             "--end-toplevel-options")
                       :output :interactive))

(defun find-inferior-image ()
  "Looks for an inferior image."
  (let* ((imgname "ks2-runner.image")
         (cwd (merge-pathnames imgname (uiop:getcwd)))
         #+sbcl
         (coreloc (merge-pathnames imgname sb-ext:*core-pathname*)))
    (cond
      ((uiop:file-exists-p cwd)
       cwd)
      #+sbcl
      ((uiop:file-exists-p coreloc)
       coreloc)
      (t
       nil))))


(defun swank-spawn ()
  "Spawns an inferior lisp with a SWANK connection"
  (let ((portfile (uiop:tmpize-pathname
                   (merge-pathnames "portfile"
                                    (uiop:temporary-directory)))))
    (delete-file portfile)
    (unwind-protect
         (let ((il-conn (spawn-inferior-lisp portfile
                                             :image (find-inferior-image))))
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
