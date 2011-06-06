;;;Try to get rid of some annoying problems with library loading(it fucks up CGI)

(defparameter *need-install* nil)

(defun fast-load (systems)
  (cond (systems
	 (handler-case (require (first systems))
	   (sb-int:extension-failure ()
	     (push (format nil "~a" (first systems)) *need-install*)))
         (fast-load (rest systems)))
	(*need-install*
	 (load "quicklisp/setup.lisp"))
	(t t)))
	  

(when reqs
  (fast-load reqs))
