;;;Try to get rid of some annoying problems with library loading(it fucks up CGI)

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
	       (quicklisp-quickstart:install :path "quicklisp/quicklisp"))
	     (setf run-already t)
	     (install-and-load))))))

(unless (find-package :ql)
  (install-and-load))

(defmacro quiet-load (systems)
  `(let ((*error-output* *standard-output*))
     (with-output-to-string (*standard-output*)
       (ql:quickload ,systems))))

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))
