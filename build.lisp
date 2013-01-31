;; http://stackoverflow.com/questions/9950680/unix-signal-handling-in-common-lisp
(defmacro set-signal-handler (signo &body body)
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
         (declare (ignore signo))
         ,@body)
       (cffi:foreign-funcall "signal" :int ,signo :pointer (cffi:callback ,handler)))))
(ccl:save-application *save-file*
		      :toplevel-function
		      (lambda ()
			(set-signal-handler 2
			  (when dfv:*proc* (ccl:signal-external-process dfv:*proc* 2))
			  (dfv:kill (dfv:getppid) 2)
			  (ccl:quit 130))
			(cfy.down-flash-video:RUN-WGET (cadr ccl:*command-line-argument-list*)))
		      :prepend-kernel t)
