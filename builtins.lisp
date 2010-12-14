;;
;;  CL-SH  -  Unix Shell in Common Lisp
;;
;;  Copyright 2010 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :cl-sh)

(defun getopts (optstring args)
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

(define-symbol-macro pwd
    *default-pathname-defaults*)

(defun pwd ()
  pwd)

(defvar oldpwd nil)

(defun cd (&rest opts-and-dir)
  (destructuring-bind (opts &optional dir) (getopts "LP" opts-and-dir)
    (assert (null (rest dir))
	    (dir) "cd: too many arguments ~S" dir)
    (setf dir (first dir))
    (cond
      ((null dir)
       (assert ($ "HOME") (($ "HOME")) "cd: no HOME")
       (setf dir (pathname (concatenate 'string ($ "HOME") "/"))))

      ((typep dir 'string)
       (when (string= dir "-")
	 (assert oldpwd (oldpwd) "cd: no OLDPWD")
	 (psetf dir oldpwd
		oldpwd dir))
       (when (string= dir "..")
	 (setf dir (make-pathname
		    :directory (butlast (pathname-directory pwd))
		    :defaults pwd))))

      (t
       (setf dir (merge-pathnames dir))))
    (when (find #\P opts)
      (setf dir (truename dir)))
    (unless (test -d dir)
      (error "cd: directory not found: ~A" dir))
    (setf pwd dir)))

(defun true ()
  0)

(defun false ()
  1)

(defvar $?
  nil)

(defun exit (&optional (status $?))
  (sb-ext:quit :unix-status status))

(defun exec ($0 &rest $*)
  (let* ((process (sb-ext:run-program $0 $*
				      :wait t
				      :input *standard-input*
				      :output *standard-output*))
	 (exit-code (sb-impl::process-exit-code
		     (sb-impl::process-wait process))))
    (exit exit-code)))
