;;;;
;;;; Hooks for image dumping and restoring
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

(defun stop-logger ()
  "Stops the global logger"
  (verbose:stop verbose:*global-controller*)
  ;; Apparently the controller won't stop unless it gets another message
  (verbose:trace :ks2.cli "Stopping global logging controller..."))

(defun start-logger ()
  "Starts the global logger"
  (verbose:start verbose:*global-controller*)
  (verbose:trace :ks2.cli "Starting global logging controller..."))

(uiop:register-image-dump-hook #'stop-logger nil)
(uiop:register-image-restore-hook #'start-logger nil)
