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


(defun probe-dir (pathname)
  (sb-posix:s-isdir (sb-posix:stat-mode (sb-posix:stat pathname))))

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
