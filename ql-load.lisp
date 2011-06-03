;;;make sure quicklisp is loaded
;;;placed in a separate file to keep everything tidy

(with-output-to-string (*standard-output*)
  (load "quicklisp.lisp"))

(defun install-fail ()
  (error "Failed to install or load quicklisp."))

(let ((run-already nil))
  (defun install-and-load ()
    (let ((init "quicklisp/setup.lisp"))
      (cond ((probe-file init)
	     (load init))
	    (run-already (install-fail))
	    (t
	     (with-output-to-string (*standard-output*)
	       (quicklisp-quickstart:install :path "quicklisp"))
	     (setf run-already t)
	     (install-and-load))))))

(unless (find-package :ql)
  (install-and-load))
