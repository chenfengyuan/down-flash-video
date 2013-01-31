(in-package :dfv)
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
			  (when *proc* (ccl:signal-external-process *proc* 2))
			  (ccl:quit 1))
			(cfy.down-flash-video:RUN-WGET (cadr ccl:*command-line-argument-list*)))
		      :prepend-kernel t)