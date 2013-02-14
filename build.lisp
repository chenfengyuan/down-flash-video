;; http://stackoverflow.com/questions/9950680/unix-signal-handling-in-common-lisp
(defmacro set-signal-handler (signo &body body)
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
         (declare (ignore signo))
         ,@body)
       (cffi:foreign-funcall "signal" :int ,signo :pointer (cffi:callback ,handler)))))
#+ccl (ccl:save-application *save-file*
			    :toplevel-function
			    (lambda ()
			      (set-signal-handler 2
						  (when (and dfv:*downloading-filename* (probe-file dfv:*downloading-filename*))
						    (delete-file dfv:*downloading-filename*))
						  (when dfv:*proc* (ccl:signal-external-process dfv:*proc* 2))
						  (dfv:kill (dfv:getppid) 2)
						  (ccl:quit 130))
			      (cfy.down-flash-video:RUN-WGET (cadr ccl:*command-line-argument-list*)))
			    :prepend-kernel t)
#+sbcl (sb-ext:save-lisp-and-die *save-file*
				 :toplevel
				 (lambda ()
				   (set-signal-handler 2
						       (when (and dfv:*downloading-filename* (probe-file dfv:*downloading-filename*))
							 (delete-file dfv:*downloading-filename*))
						       (when dfv:*proc* (dfv:kill (sb-ext:process-pid dfv:*proc*) 2))
						       (dfv:kill (dfv:getppid) 2)
						       (sb-ext:exit :code 130 :abort t))
				   (cfy.down-flash-video:RUN-WGET (cadr sb-ext:*posix-argv*)))
				 :executable t
				 :compression 9)
