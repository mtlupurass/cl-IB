;;;make sure quicklisp is loaded
;;;placed in a separate file to keep everything tidy

(defun install-fail ()
  (error "Failed to install or load quicklisp."))

(let ((run-already nil))
  (defun install-and-load ()
    (let ((init (merge-pathnames "quicklisp/setup.lisp"
				 (user-homedir-pathname))))
      (cond ((probe-file init)
	     (load init))
	    (run-already (install-fail))
	    (t
	     (with-output-to-string (*standard-output*)
	       (or (load "quicklisp.lisp")
		   (install-fail))
	       (quicklisp-quickstart:install))
	     (setf run-already t)
	     (install-and-load))))))
#-quicklisp
(install-and-load)
