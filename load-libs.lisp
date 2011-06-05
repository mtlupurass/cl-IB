;;;Try to get rid of some annoying problems with library loading(it fucks up CGI)

;;quiet-load: loads a library using quicklisp, suppressing output.
(defun quiet-load (systems)
  (let ((*error-output* *standard-output*))
    (with-output-to-string (*standard-output*)
      (eval '(ql:quickload systems)))))

;;fast-load: quicklisp has an annoying tendency of downloading a bunch of crap all the time. Try require first to save resources.
(defun fast-load (system)
  (handler-case (require system)
    (sb-int:extension-failure ()
      (quiet-load (format nil "~a" system)))))

(when reqs
  (map 'list #'fast-load reqs))
