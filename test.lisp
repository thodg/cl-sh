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

(defun probe-dir (pathname)
  (when (pathname-name pathname)
    (warn "(PROBE-DIR ~A) : pathname is not a directory" pathname))
  (loop
     with dir = (pathname-directory pathname)
     for i from 1 upto (length dir)
     do (let ((newpath (make-pathname :host (pathname-host pathname)
				      :device (pathname-device pathname)
				      :directory (subseq dir 0 i))))
	  (unless (probe-file newpath)
	    (return nil)))
     finally (return t)))

(defun test-1 (op arg)
  (ecase op
    ((-d) `(probe-dir ,arg))))

(defun test-2 (op arg1 arg2)
  (ecase op
    ((-&& -and)
     `(and ,arg1 ,arg2))))

(defun test-n (op first-arg &rest more-args)
  (let ((one-test (test-1 op first-arg)))
    (if more-args
	(test-2 (first more-args) one-test (apply #'test-n (rest more-args)))
	one-test)))

(defmacro test (op first-arg &rest more-args)
  (apply #'test-n op first-arg more-args))
