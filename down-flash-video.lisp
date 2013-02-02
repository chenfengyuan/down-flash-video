;; Copyright (C) 2012 Chen Fengyuan (jeova.sanctus.unus+po2db (at) gmail.org)
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
(in-package :cl)
(declaim (optimize (speed 3)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  #+ccl
  (progn
    (setf (CCL:PATHNAME-ENCODING-NAME) :utf-8)
    (setf ccl:*default-external-format* :utf-8)
    (setf ccl:*default-file-character-encoding* :utf-8))
  (dolist (p '(:drakma :cl-mechanize :cl-base64 :cl-ppcre :cffi))
    (unless (find-package p)
      (ql:quickload p))))
(defpackage :cfy.down-flash-video
  (:nicknames :dfv)
  (:use :cl :cl-mechanize)
  (:import-from :cl-base64 :string-to-base64-string)
  (:import-from :drakma :http-request)
  (:import-from :babel :octets-to-string)
  (:import-from :flexi-streams :string-to-octets)
  (:export :run-wget :*proc* :getppid :kill :*downloading-filename*))
(in-package :cfy.down-flash-video)
(defun getppid nil
  (cffi:foreign-funcall "getppid" :int))
(defun kill (pid signal)
  (cffi:foreign-funcall "kill" :int pid :int signal :int))
(defparameter *proc* nil)
(defparameter *downloading-filename* nil)
(defparameter *download-count* 0)
(defparameter *log-file* #+ccl"down-flash-video.log" #+sbcl "/down-flash-video.log")
(defparameter *user-agent* "Opera/9.80 (X11; Linux x86_64; U; en) Presto/2.10.289 Version/12.01"
  "the user agent is given to the flvcd")
(defparameter *flvcd-format* "http://www.flvcd.com/parse.php?&format=~a&kw=~a"
  "the url format")
(defparameter *downloader* "wget")
(defparameter *total-time-print-p* nil)
(defun flvxz-format (video-url)
  (format nil "http://www.flvxz.com/getFlv.php?url=~a"
	  (string-to-base64-string video-url)))
(defun get-download-urls-from-flvxz (video-url &key (user-agent *user-agent*))
  (http-request (flvxz-format video-url) :user-agent user-agent :external-format-in :utf-8 :external-format-out :utf-8))

(declaim (inline get-uri-string))
(defun get-uri-string (uri)
  (princ-to-string uri))
(declaim (inline drakma-gbk-convert))
(defun get-program-output (program &optional args)
  (with-output-to-string (out)
    #+ccl
    (ccl:run-program program args
		     :output out
		     :wait t)
    #+sbcl
    (sb-ext:run-program program args
			:output out
			:wait t
			:search t)))
(defun drakma-gbk-convert (str)
  (octets-to-string
   (string-to-octets
    str :external-format :latin-1) :encoding :gbk))
(defun get-download-urls-and-name (video-url &key (format "super") (user-agent *user-agent*) (flvcd-format *flvcd-format*))
  (let* ((browser (make-instance 'browser :user-agent user-agent))
	 (page (fetch (format nil flvcd-format format video-url) browser))
	 (links (loop for link in (page-links page)
		      if (search "http://" (link-text link))
		      collect (get-uri-string (link-uri link))))
	 (name (format
		nil "~a/"
		(elt
		 (cadr
		  (multiple-value-list
		   (cl-ppcre:scan-to-strings
		    "document.title = \"([^\"]+)\""
		    (drakma-gbk-convert (page-content page)))))
		 0))))
    (cons links name)))
(declaim (inline output-name-format))
(defun output-name-format (links)
  (let* ((digits (ceiling (log (length links) 10)))
	 (name-format (format nil "~~~d,'0d.flv" digits)))
    name-format))
(defun get-arguments-list (links)
  (let ((name-format (output-name-format links)))
    (loop
	  for i from 0
	  for l in links
	  collect (list "-nc" "--user-agent=Opera/9.80 (X11; Linux i686; U; en) Presto/2.6.30 Version/10.60" l "-O" (format nil name-format i)))))
(defun wget-size (arg)
  (pop arg)
  (setf arg (list "--spider" (nth 0 arg) (nth 1 arg) "-O" "/dev/null"))
  ;; can't use tsocks here.....use wget directly....
  (let* ((r (get-program-output "wget" arg))
	 (size (multiple-value-bind (all size)
		   (cl-ppcre:scan-to-strings "Length:\\s+(\\d+)" r)
		 (declare (ignore all))
		 (if (> (length size) 0)
		     (aref size 0)
		     nil))))
    (if (stringp size)
	(parse-integer size))))
(defun get-file-size (file)
  (with-open-file (in file)
    (file-length in)))
(defun full-download-p (arg)
  (let* ((wget-size (wget-size arg))
	 (file #+sbcl (sb-ext:parse-native-namestring (concatenate 'string (sb-posix:getcwd) "/" (car (last arg)))) #+ccl (car (last arg)))
	 (file-size (if (probe-file file)
			(get-file-size file)
			0)))
    (= file-size wget-size)))
(defun wget (arg hook)
  (incf *download-count*)
  (if arg
      (progn
	(setf *total-time-print-p* t)
	(format t "downloading the ~:r now." *download-count*)
	(finish-output))
      (setf *total-time-print-p* nil))
  #+ccl (ccl:run-program *downloader* arg
			    :output *log-file*
			    :wait nil
			    :status-hook hook
			    :if-output-exists :append)
  #+sbcl (sb-ext:run-program *downloader* arg
				:output *log-file*
				:wait nil
				:status-hook hook
				:if-output-exists :append
				:search t))
(defun url-filter (url)
  (let ((p (position #\? url)))
    (if p
	(subseq url 0 p)
	url)))
(defun run-wget (video-url)
  (let* ((result (let ((r (get-download-urls-and-name (url-filter video-url))))
		   (if (car r)
		       r
		       (progn
			 (format t "~a~%"
				 (cond
				   ((string= (cdr r) "FLVCD硕鼠官网|FLV下载/") "The url doesn't seem correct.")
				   (t "It seems that FLVCD can't parse the url.")))
			 #+ccl (ccl:quit 2)
			 #+sbcl (sb-ext:exit :code 2)))))
	 (name (cdr result))
	 (links (car result))
	 (args (get-arguments-list links))
	 (time (get-universal-time))
	 (path (namestring #+ccl (ccl:cwd ".")
			   #+sbcl (sb-posix:getcwd))))
    (when #+ccl (ccl:getenv "DOWNLOADER")
	  #+sbcl (sb-posix:getenv "DOWNLOADER")
      (setf *downloader*
	    #+ccl (ccl:getenv "DOWNLOADER")
	    #+sbcl (sb-posix:getenv "DOWNLOADER"))
      (format t "Use downloader:~a~%" *downloader*)
      (finish-output))
    (setf *log-file* (concatenate 'string path *log-file*))
    (or (probe-file #+ccl *log-file* #+sbcl (sb-ext:parse-native-namestring *log-file*))
	(close (open *log-file* :direction :output :if-does-not-exist :create)))
    (ensure-directories-exist #+sbcl (sb-ext:parse-native-namestring name) #+ccl name)
    #+ccl (ccl:cwd name)
    #+sbcl (sb-posix:chdir name)
    (format t "downloading ~a~%total ~a,log: tail -f ~a~%Checking file size~%" name (length args) *log-file*)
    (finish-output)
    (loop with new-args = nil
	  for arg in args
	  for file = #+sbcl (sb-ext:parse-native-namestring (concatenate 'string (sb-posix:getcwd) "/" (car (last arg)))) #+ccl (car (last arg))
	  for i from 0
	  do (if (probe-file file)
		 (if (full-download-p arg)
		     (push nil new-args)
		     (progn
		       (format t "the file length of ~:r is not correct.Deleteing it~%" i)
		       (finish-output)
		       (delete-file file)
		       (push arg new-args)))
		 (push arg new-args))
	  finally (setf args (nreverse new-args)))
    (labels ((rerun-wget (x)
    	       (case #+ccl (ccl:external-process-status x)
		     #+sbcl (sb-ext:process-status x)
    		 (:exited
		  (when *total-time-print-p*
		    (format t "total time: ~d ~%" (- (get-universal-time) time)))
		  (finish-output *standard-output*)
		  (setf time (get-universal-time))
		  (if args
		      (let ((arg (pop args)))
			(setf
			 *downloading-filename* #+sbcl (sb-ext:parse-native-namestring (concatenate 'string (sb-posix:getcwd) "/" (car (last arg)))) #+ccl (car (last arg))
			 *proc* (wget arg #'rerun-wget)))
		      #+ccl (ccl:quit)
		      #+sbcl (sb-ext:exit)))
		 (otherwise (progn
			      (dfv:kill (dfv:getppid) 2)
			      (when (and *downloading-filename* (probe-file dfv:*downloading-filename*))
				(delete-file *downloading-filename*))
			      #+ccl (ccl:quit 130)
			      #+sbcl (sb-ext:exit :code 130))))))
      (let ((arg (pop args)))
			(setf
			 *downloading-filename* #+sbcl (sb-ext:parse-native-namestring (concatenate 'string (sb-posix:getcwd) "/" (car (last arg)))) #+ccl (car (last arg))
			 *proc* (wget arg #'rerun-wget))))
    (loop (sleep 99999))))
(defun hello (str)
  (format t "~a,测试2~%" str))

