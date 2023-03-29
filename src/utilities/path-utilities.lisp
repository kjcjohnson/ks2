;;;;
;;;; Utilities for path
;;;;
(in-package #:com.kjcjohnson.ks2.utilities)

;;; Path variable handling
(defun %path-separator ()
  (if (uiop:os-windows-p)
      #\;
      #\:))

(defun path-variable ()
  "Gets the contents of the PATH variable as a list"
  (let* ((path (uiop:getenv "PATH"))
         (split-path (str:split (%path-separator) path :omit-nulls t)))
    (map-into split-path
              (a:rcurry #'uiop:parse-native-namestring :ensure-directory t)
              split-path)))
         
(defun core-location ()
  "Attempts to get the location of the running Lisp core. Currently only on SBCL..."
  #+sbcl
  (let ((coreloc sb-ext:*core-pathname*))
    (make-pathname
     :directory (remove-if (a:curry #'string= "") (pathname-directory coreloc))
     :device (pathname-device coreloc)
     :host (pathname-host coreloc)))
  #-sbcl
  nil)

(defun locate-exe (filename)
  "Locates an executable named FILENAME. The .exe suffix may be added on Windows."
  (if (uiop:os-windows-p)
      (locate-file filename :optional-suffix "exe")
      (locate-file filename)))

(defun locate-file (filename &key optional-suffix hint-path
                               (use-path t) (use-coreloc t) (use-cwd nil))
  "Locates FILENAME, maybe with OPTIONAL-SUFFIX, in the path, next to the core, or at
a specified hint path. Returns full path to file."
  (flet ((maybe-file (base-pathname)
           (when base-pathname
             (let ((path (merge-pathnames filename base-pathname)))
               #+()(format t "Check: ~a~%" path)
               (when (uiop:file-exists-p path)
                 (return-from locate-file path)))
             (when optional-suffix
               (let ((path (merge-pathnames (make-pathname :name filename
                                                           :type optional-suffix)
                                            base-pathname)))
                 #+()(format t "Check: ~a~%" path)
                 (when (uiop:file-exists-p path)
                   (return-from locate-file path)))))))
    ;; First, use the hint path
    (when hint-path
      (maybe-file hint-path))

    ;; Then check CWD
    (when use-cwd
      (maybe-file (uiop:getcwd)))

    ;; Then relative to core
    (when use-coreloc
      (maybe-file (core-location)))

    ;; Then check all path entries
    (when use-path
      (loop for path-entry in (path-variable)
            do (maybe-file path-entry)))

    ;; Otherwise, nil
    nil))
