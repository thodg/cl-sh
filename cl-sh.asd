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

(defpackage :cl-sh.system
  (:use :cl :asdf))

(in-package :cl-sh.system)

(defsystem :cl-sh
  :name "cl-sh"
  :author "Thomas de Grivel <billitch@gmail.com>"
  :version "0.1"
  :description "Unix Shell in Common Lisp"
  :depends-on ("cl-fad" "sb-posix")
  :components
  ((:file "defpackage")
   (:file "test" :depends-on ("defpackage"))
   (:file "sh" :depends-on ("defpackage" "test"))
   (:file "builtins" :depends-on ("defpackage" "sh"))))
