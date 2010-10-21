;;
;;  CL-WHOIS  -  WHOIS Protocol in Common Lisp
;;
;;  Copyright 2010 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission is hereby granted, free of charge, to any person
;;  obtaining a copy of this software and associated documentation
;;  files (the "Software"), to deal in the Software without
;;  restriction, including without limitation the rights to use, copy,
;;  modify, merge, publish, distribute, sublicense, and/or sell copies
;;  of the Software, and to permit persons to whom the Software is
;;  furnished to do so, subject to the following conditions:
;;  
;;  The above copyright notice and this permission notice shall be
;;  included in all copies or substantial portions of the Software.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;  DEALINGS IN THE SOFTWARE.
;;

(in-package :cl-sh)

(defvar oldpwd nil)

(define-symbol-macro pwd
    *default-pathname-defaults*)

(defun pwd ()
  pwd)

(defun $ (name)
  (sb-posix:getenv name))

(defsetf $ (name) (value)
  (let ((v (gensym "V")))
    `(let ((,v ,value))
       (sb-posix:setenv ,name ,v 1)
       ,v)))

(defun getopt (optstring args)
  (labels ((munch-string (string index opts)
	     (declare (type string string)
		      (type fixnum index))
	     (let ((c (char string index)))
	       (cond ((= index 0)
		      opts)
		     ((assert (find c optstring)
			      (c optstring)
			      "Invalid option char: ~C" c))
		     (t
		      (munch-string string
				    (1- index)
				    (cons c opts))))))
	   (munch-args (opts list)
	     (if list
		 (let ((arg (string (first list))))
		   (cond ((string= "--" arg)
			  (list opts (rest list)))
			 ((char= #\- (char arg 0))
			  (munch-args (munch-string arg
						    (1- (length arg))
						    opts)
				      (rest list)))
			 (t
			  (list opts list))))
		 (cons opts nil))))
    (munch-args nil args)))

(defun cd (&rest opts-and-dir)
  (destructuring-bind (opts &optional dir) (getopt "LP" opts-and-dir)
    (assert (null (rest dir))
	    (dir) "cd: too many arguments ~S" dir)
    (setf dir (first dir))
    (when (null dir)
      (assert ($ "HOME") (($ "HOME")) "cd: no HOME")
      (setf dir (make-pathname :directory ($ "HOME"))))
    (when (and (typep dir 'string)
	       (string= dir "-"))
      (assert oldpwd (oldpwd) "cd: no OLDPWD")
      (psetf dir oldpwd
	     oldpwd dir))
    (setf dir (merge-pathnames dir))
    (when (find #\P opts)
      (setf dir (truename dir)))
    (unless (test -d dir)
      (error "cd: directory not found: ~A" dir))
    (setf pwd dir)))
