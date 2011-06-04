;;;Try to get rid of some annoying problems with library loading(it fucks up CGI)

(defun install-fail ()
  (error "Failed to install or load quicklisp."))

;;quiet-load: loads a library using quicklisp, suppressing output.
(defun quiet-load (systems)
  (let ((*error-output* *standard-output*))
    (with-output-to-string (*standard-output*)
      (ql:quickload systems))))

;;fast-load: quicklisp has an annoying tendency of downloading a bunch of crap all the time. Try require first to save resources.
(defun fast-load (system)
  (handler-case (require system)
    (sb-int:extension-failure ()
      (quiet-load (format nil "~a" system)))))

(defun load-quicklisp ()
  (unless (find-package :quicklisp)
    (let ((init "quicklisp/setup.lisp"))
      (when (probe-file init)
	(load init))))

  (unless (find-package :quicklisp)
    (with-output-to-string (*standard-output*)
      (load "quicklisp.lisp")))

  (unless (find-package :quicklisp)
    (install-fail))
  t)

(map #'fast-load reqs)
