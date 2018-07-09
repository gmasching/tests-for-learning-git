(defpackage #:stuff
  (:use #:cl #:utility))

(in-package #:stuff)

;;;;Convert c names to lisp names:
;;;;each captial letter in the c name gets a "-" behind it
;;;;each lowercase letter gets uppercased

;;;;Convert lisp names to c names:
;;;;make all letters lowercased except the ones with a "-" behind it and remove the "-"s

(defun copy-string (x)
  (map 'string (function identity) x))

(defparameter *buf* (make-array 0 :fill-pointer 0 :adjustable t :element-type 'character))
(defmacro with-output-to-buf ((&optional (fun 'add) (buf '*buf*)) &body body)
  (once-only (buf)
    `(progn
       (setf (fill-pointer ,buf) 0)
       (flet ((,fun (char)
		(vector-push-extend char ,buf)))
	 ,@body)
       (copy-string ,buf))))
(defun c-lisp-name (thing)
  (with-output-to-buf (add *buf*)
    (map nil
	 (lambda (char)
	   (cond ((upper-case-p char)
		  (add #\-)
		  (add char))
		 ((lower-case-p char)
		  (add (char-upcase char)))
		 (t (add char))))
	 (string thing))))
(defun lisp-c-name (thing)
  (with-output-to-buf (add *buf*)
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
		   (add (if after--
			    (char-upcase char)
			    (char-downcase char))))
		 (incf index)
		 (when (>= index len)
		   (go end))
		 (setf after-- no-char))
	       (go start)
	     end))))))
(defun test-c-lisp (str)
  (let* ((afoo (c-lisp-name str))
	 (a (lisp-c-name afoo))
	 (test1 (string= str a)))
    (if test1
	(format t "c->lisp: ok")
	(format t "~&1: ***FAILED***:~&c->lisp ~a~&lisp->c ~a" afoo a))
    test1))

(defun test-lisp-c (str)
  (let* ((bfoo (lisp-c-name str))
	 (b (c-lisp-name bfoo))
	 (test2 (string= str b)))
    (if test2
	(format t "lisp->c: ok")
	(format t "~&: ***FAILED***:~&lisp->c ~a~&c->lisp ~a" bfoo b))
    test2))
