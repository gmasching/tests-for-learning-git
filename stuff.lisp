(defpackage #:stuff
  (:use #:cl #:utility))

(in-package #:stuff)

(defun copy-string (x)
  (map 'string (function identity) x))

(defparameter *buf* (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
(defun c-lisp-name (thing)
  (setf (fill-pointer *buf*) 0)
  (let ((str (string thing)))
    )
  (copy-string *buf*))
(defun lisp-c-name (thing)
  (setf (fill-pointer *buf*) 0)
  (let* ((str (string thing))
	 (len (length str)))
    (if (zerop len)
	(error "c names cannot be empty")
	(let ((after-- nil)
	      (char nil)
	      (index 0))
	  (tagbody
	     start
	     (setf char (aref str index))
	     (let ((no-char (char-equal #\- char)))
	       (unless no-char
		 (if after--
		     (vector-push-extend (char-upcase char)
					 *buf*)
		     (vector-push-extend (char-downcase char)
					 *buf*)))
	       (incf index)
	       (when (>= index len)
		 (go end))
	       (setf after-- no-char))
	     (go start)
	     end))))
  (copy-string *buf*))
