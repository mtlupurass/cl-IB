#!/usr/bin/sbcl --script
;;; main program. Needs execution rights.
;;;
;;;


(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(handler-bind ((warning #'ignore-warning))
  (load "default-config.lisp")
  (load "load-libs.lisp")
  (load "strings.lisp")
  (load "cgi.lisp")
  (load "template.lisp")
  (use-package '(:cgi :ib-templates)))

(load-template-file "markup")

(defun make-http-header ()
  (format t "Content-type: text/html~%~%"))

(defun print-page (s)
  (make-http-header)
  (format t "~a" s))

(print-page (compile-template 'TEST))
