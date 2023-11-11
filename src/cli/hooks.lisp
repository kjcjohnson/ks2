;;;;
;;;; Hooks for image dumping and restoring
;;;;
(in-package #:com.kjcjohnson.ks2.cli)

(defun stop-logger ()
  "Stops the global logger"
  (v:stop v:*global-controller*)
  ;; Apparently the controller won't stop unless it gets another message
  (v:trace :ks2.cli "Stopping global logging controller...")
  (sleep 1))

(defun start-logger ()
  "Starts the global logger"
  (v:start v:*global-controller*)
  (v:trace :ks2.cli "Starting global logging controller..."))

(uiop:register-image-dump-hook #'stop-logger nil)
(uiop:register-image-restore-hook #'start-logger nil)
(exit-hooks:add-exit-hook #'stop-logger)
