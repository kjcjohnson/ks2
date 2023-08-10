;;;;
;;;; KS2 runner
;;;;
(in-package #:com.kjcjohnson.ks2.runner)

(defun spawn-inferior-lisp (port-file &key image (output :interactive))
  "Spawns an inferior lisp. Currently, just SBCL."
  (debug-msg "===== SPAWNING CORE =====~% - PORT-FILE: ~a~% - IMAGE: ~a~%"
             port-file image)
  (uiop:launch-program (list (if (null image)
                                 "sbcl"
                                 image)
                             "--disable-ldb"
                             "--lose-on-corruption"
                             "--dynamic-space-size"
                             "8192" ; MiB
                             "--end-runtime-options"

                             "--eval"
                             (if (null image)
                                 "(asdf:oos 'asdf:load-op \"com.kjcjohnson.ks2.runner/helper\")"
                                 "(push :ks2-bootstrapped *features*)")
                             "--eval"
                             (format nil
                                     "(com.kjcjohnson.ks2.runner.helper:init-and-start-swank ~s)"
                                     port-file)

                             "--end-toplevel-options")
                       :output (or output :interactive)))

(defun find-inferior-image ()
  "Looks for an inferior image."
  (let* ((imgname "ks2-core.image")
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

(defun %maybe-process-characters (stream callback)
  (loop for c = (read-char-no-hang stream nil)
        unless c do (return) end
          when (and c callback)
            do (funcall callback (make-string 1 :initial-element c))))

(defun swank-spawn (&key output output-callback error error-callback status-callback)
  "Spawns an inferior lisp with a SWANK connection"
  (let ((portfile (uiop:tmpize-pathname
                   (merge-pathnames "portfile"
                                    (uiop:temporary-directory)))))
    (when (uiop:file-exists-p portfile)
      (delete-file portfile))
    (unwind-protect
         (let ((il-conn (spawn-inferior-lisp portfile
                                             :image (find-inferior-image)
                                             :output output)))
           (debug-msg "===== WAITING FOR SWANK =====")
           (when status-callback
             (funcall status-callback "Waiting for SWANK connection..."))
           (loop
             for i from 0 to 30
             doing (format t ".")
             doing (force-output)
             doing (sleep 1)
             doing (when (eql :stream output)
                     (%maybe-process-characters
                      (uiop:process-info-output il-conn)
                      output-callback))
             doing (when (eql :stream error)
                     (%maybe-process-characters
                      (uiop:process-info-error-output il-conn)
                      error-callback))
             until (uiop:file-exists-p portfile)
             finally (terpri))

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
               (debug-msg "===== CORE SPAWNED =====")
               spawn)))
      (when (uiop:file-exists-p portfile)
        (delete-file portfile)))))
