;;;Try to get rid of some annoying problems with library loading(it fucks up CGI)

;;fast-load: quicklisp has an annoying tendency of downloading a bunch of crap all the time. Try require first to save resources.

(defparameter *need-install* nil)

(defun fast-load (systems)
  (cond (systems
	 (handler-case (require system)
	   (sb-int:extension-failure ()
	     (push (format nil "~a" system) *need-install*))))
	(*need-install*
	 (load "quicklisp/setup.lisp"))
	(t t)))
	  

(when reqs
  (fast-load reqs))
