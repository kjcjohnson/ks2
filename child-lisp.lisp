;;;;
;;;; Child Lisp functionality
;;;;
(in-package #:com.kjcjohnson.ks2.runner)

(defclass child-lisp ()
  ((process
    :initarg :process
    :reader child-lisp-process
    :documentation "The UIOP process object for the inferior lisp")
   (port
    :initarg :port
    :reader child-lisp-port
    :documentation "The port where SWANK is connected on the child lisp")
   (swank-connection
    :initarg :swank-connection
    :reader child-lisp-swank-connection
    :documentation "The connection to SWANK for the child")
   (continuation-table
    :initarg :continuation-table
    :reader child-lisp-continuation-table
    :documentation "Mapping between SWANK continuations and functions to call")
   (immediate-result-table
    :initarg :immediate-result-table
    :reader child-lisp-immediate-result-table
    :documentation "Mapping between SWANK continuations and results with 
no registered continuation. Useful if the response arrives early.")
   (result-lock
    :initarg :result-lock
    :reader child-lisp-result-lock
    :documentation "Lock for the continuation and immediate tables")
   (response-thread
    :reader child-lisp-response-thread
    :writer (setf %child-lisp-response-thread)
    :documentation "Thread processing SWANK responses")
   (connection-error
    :initarg :connection-error
    :reader child-lisp-connection-error
    :writer (setf %child-lisp-connection-error)
    :documentation "Flag to signal if the SWANK connection is broken or errored."))
  (:default-initargs
   :process (error "Process is required.")
   :port (error "Port is required.")
   :swank-connection "SWANK connection is required."
   :continuation-table (make-hash-table)
   :immediate-result-table (make-hash-table)
   :result-lock (bt:make-recursive-lock)
   :connection-error nil)
  (:documentation "Holder for information about a running child lisp process
and its connection via SWANK."))

(defun %make-error-response (message)
  "Creates an error response."
  `(:error ,message))

(defun %make-timeout-response (message)
  "Creates a timeout response."
  `(:timeout ,message))

(defun %make-crash-response (message)
  "Creates a crash response."
  `(:crash ,message))

(defun %send-rpc (child-lisp form)
  "Sends an RPC over the SWANK connection. Returns the continuation count."
  (let ((swank (child-lisp-swank-connection child-lisp)))
    (swank-protocol:emacs-rex swank form)
    (swank-protocol:connection-request-count swank)))

(defun %register-continuation (child-lisp continuation fn-to-execute)
  "Registers a continuation with a function to call when SWANK reply received."
  (setf (gethash continuation (child-lisp-continuation-table child-lisp))
        fn-to-execute))

(defun %lookup-immediate-result (child-lisp continuation)
  "Checks if an immediate result is available for the given SWANK continuation."
  (multiple-value-bind (result found?)
      (gethash continuation (child-lisp-immediate-result-table child-lisp))
    (when found?
      (remhash continuation (child-lisp-immediate-result-table child-lisp)))
    (values result found?)))

(defun %lookup-continuation (child-lisp continuation)
  "Looks up a continuation in the continuation table. Returns the function or NIL
if the continuation is not registered in the table yet."
  (multiple-value-bind (fn found?)
      (gethash continuation (child-lisp-continuation-table child-lisp))
    (when found?
      (remhash continuation (child-lisp-continuation-table child-lisp)))
    (values fn found?)))

(defun %register-immediate-result (child-lisp continuation result)
  "Registers an immediate result."
  (setf (gethash continuation (child-lisp-immediate-result-table child-lisp))
        result))

(defun %process-continuation-response (child-lisp continuation result)
  "Processes an RPC response that has an associated continuation."
  (let ((lock (child-lisp-result-lock child-lisp))
        (continuation-fn nil)
        (continuation-fn-found? nil))
    (bt:with-recursive-lock-held (lock)
      (multiple-value-bind (fn found?)
          (%lookup-continuation child-lisp continuation)
        (if found?
            (setf continuation-fn fn
                  continuation-fn-found? t)
            (%register-immediate-result child-lisp continuation result))))
    (when continuation-fn-found?
      (funcall continuation-fn result))))

(defun %respond-all-continuations (child-lisp response)
  "Signals errors in all waiting continuations."
  (let ((lock (child-lisp-result-lock child-lisp)))
    (bt:with-recursive-lock-held (lock)
      (loop for continuation being
              the hash-keys of (child-lisp-continuation-table child-lisp)
                using (hash-value continuation-fn)
            do (%process-continuation-response child-lisp
                                               continuation
                                               response))))) 

(defun %process-swank-response (child-lisp response)
  "Processes a single SWANK response."
  (case (first response)
    (:return
      (%process-continuation-response child-lisp
                                      (third response)
                                      (second response)))
    (:debug
     (format *trace-output* "; ERROR IN CHILD LISP~%")
     (dolist (msg (fourth response))
       (format *trace-output* "; --> ~a~%" msg))
     (dolist (frame (sixth response))
       (format *trace-output* ";  FRAME [~s]: ~s~%" (first frame) (second frame)))
     (dolist (continuation (seventh response))
       (%process-continuation-response child-lisp
                                       continuation
                                       (%make-error-response "Error in RPC"))))
    (:debug-activate
     (rpc-call/sync child-lisp '(swank:throw-to-toplevel)))
    (:new-features nil)
    (otherwise
     (format t "; OTHER MESSAGE: ~s~%" response))))

(defun %swank-response-loop (child-lisp)
  "Processes SWANK responses."
  (handler-case
      (loop
        for swc = (child-lisp-swank-connection child-lisp)
        while swc
        do (%process-swank-response
            child-lisp
            (swank-protocol:read-message swc)))
    (error (e)
      (setf (%child-lisp-connection-error child-lisp) t)
      (format *trace-output* "; SWANK ERROR: ~a~%~%" e)
      (force-output *trace-output*)
      (%respond-all-continuations child-lisp
                                  (%make-crash-response
                                   "SWANK response thread crash")))))

(defun init-swank-response-loop (child-lisp)
  "Starts a thread for processing SWANK responses."
  (setf (%child-lisp-response-thread child-lisp)
        (bt:make-thread #'(lambda () (%swank-response-loop child-lisp)))))

(defun %rpc-call (child-lisp form continuation-fn)
  "Makes an RPC call and registers a function to be executed on return."
  (let ((continuation (handler-case (%send-rpc child-lisp form)
                        (error ()
                          (funcall continuation-fn
                                   (%make-crash-response "Unable to call RPC."))
                          (return-from %rpc-call)))))
    (let ((lock (child-lisp-result-lock child-lisp))
          (immediate-result nil)
          (immediate-result-found? nil))
      (bt:with-recursive-lock-held (lock)
        ;; It is possible that the result is already available
        (multiple-value-bind (result found?)
            (%lookup-immediate-result child-lisp continuation)
          (when (child-lisp-connection-error child-lisp)
            (setf result (%make-crash-response "SWANK response thread crash")
                  found? t))
          (if found?
            (setf immediate-result result
                  immediate-result-found? t)
            (%register-continuation child-lisp continuation continuation-fn))))
      (when immediate-result-found?
        (funcall continuation-fn immediate-result)))))

(defun rpc-call/promise (child-lisp form)
  "Calls an RPC and returns a promise that is fulfilled when the RPC completes."
  (let ((promise (lparallel:promise)))
    (%rpc-call child-lisp form #'(lambda (ret-val)
                                   (lparallel:fulfill promise ret-val)))
    promise))
  
(defun rpc-call/sync (child-lisp form)
  "Calls an RPC and blocks until the return value is available"
  (let ((promise (lparallel:promise)))
    (%rpc-call child-lisp form #'(lambda (ret-val)
                                   (lparallel:fulfill promise ret-val)))
    (lparallel:force promise)))

(defun rpc-call/async (child-lisp form callback)
  "Calls an RPC and calls CALLBACK with the result."
  (%rpc-call child-lisp form callback))

(defun terminate-child (child-lisp &key urgent)
  "Ends a child lisp session."
  (when (bt:thread-alive-p (child-lisp-response-thread child-lisp))
    (bt:destroy-thread (child-lisp-response-thread child-lisp))) ; DANGEROUS! FIXME
  (uiop:terminate-process (child-lisp-process child-lisp) :urgent urgent))
