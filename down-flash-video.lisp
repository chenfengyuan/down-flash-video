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
(declaim (optimize (debug 3)))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (CCL:PATHNAME-ENCODING-NAME) :utf-8)
  (setf ccl:*default-external-format* :utf-8)
  (setf ccl:*default-file-character-encoding* :utf-8)
   (dolist (p '(:drakma :cl-mechanize :cl-base64 :cl-ppcre))
    (unless (find-package p)
      (ql:quickload p))))
(defpackage :cfy.down-flash-video
  (:nicknames :dfv)
  (:use :cl :cl-mechanize)
  (:import-from :cl-base64 :string-to-base64-string)
  (:import-from :drakma :http-request)
  (:import-from :babel :octets-to-string)
  (:import-from :flexi-streams :string-to-octets))
(in-package :cfy.down-flash-video)
(defparameter *log-file* "/tmp/down-flash-video.log")
(defparameter *user-agent* "Opera/9.80 (X11; Linux x86_64; U; en) Presto/2.10.289 Version/12.01"
  "the user agent is given to the flvcd")
(defparameter *flvcd-format* "http://www.flvcd.com/parse.php?&format=~a&kw=~a"
  "the url format")
(defun flvxz-format (video-url)
  (format nil "http://www.flvxz.com/getFlv.php?url=~a"
	  (string-to-base64-string video-url)))
(defun get-download-urls-from-flvxz (video-url &key (user-agent *user-agent*))
  (http-request (flvxz-format video-url) :user-agent user-agent :external-format-in :utf-8 :external-format-out :utf-8))

(declaim (inline get-uri-string))
(defun get-uri-string (uri)
  (princ-to-string uri))
(declaim (inline drakma-gbk-convert))
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
	 (name-format (format nil "~~~d,'0d" digits)))
    name-format))
(defun get-arguments-list (links)
  (let ((name-format (output-name-format links)))
    (loop
	  for i from 0
	  for l in links
	  collect (list "-nc" "--user-agent=Opera/9.80 (X11; Linux i686; U; en) Presto/2.6.30 Version/10.60" l "-O" (format nil name-format i)))))
(let ((count 0))
  (defun wget (arg hook)
    (format t "正在下载第~d个~%" (incf count))
    (ccl:run-program "wget" arg
		     :output *log-file*
		     :wait nil
		     :status-hook hook
		     :if-output-exists :supersede)))
(defun run-wget (video-url)
  (let* ((result (get-download-urls-and-name video-url))
	 (name (cdr result))
	 (links (car result))
	 (args (get-arguments-list links))
	 proc)
    (ensure-directories-exist name)
    (ccl:cwd name)
    (format t "开始下载 ~a~%总共~a个,log:~a~%" name (length args) *log-file*)
    (finish-output *standard-output*)
    (labels ((rerun-wget (x)
    	       (case (ccl:external-process-status x)
    		 (:exited
    		  (when args
    		    (finish-output *standard-output*)
    		    (setf proc
    			  (wget (pop args) #'rerun-wget)))))
    	       (unless args
    		 (ccl:quit))))
      (setf proc (wget (pop args) #'rerun-wget)))
    (ccl:wait-for-signal 2 nil)
    (ccl:signal-external-process proc 2)
    (ccl:quit 1)))
(defun hello (str)
  (format t "~a,测试2~%" str))
