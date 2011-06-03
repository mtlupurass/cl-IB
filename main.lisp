#!/usr/bin/sbcl --script
;;; main program. Needs execution rights.
;;;
;;;


(load "ql-load.lisp") ;properly load(or install) quicklisp
(hander-bind ((warning #'ignore-warning))
  (load "default-config.lisp")
  (load "strings.lisp")
  (load "cgi.lisp")
  (load "template.lisp")
  (use-package '(:cgi :ib-templates)))

(load-template-file "markup")
(print-page (compile-template 'TEST))

(defun make-http-header ()
  (format t "Content-type: text/html~%~%"))

(defun print-page (s)
  (make-http-header)
  (format t "~a" s))
