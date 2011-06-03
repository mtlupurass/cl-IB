(defpackage :ib-cgi
  (:use
   :cl)
  (:nicknames :cgi)
  (:export
   :getenv
   :query
   :cookie
   :set-cookie)
  (:import-from :cl-user :quiet-load))
  
(in-package "IB-CGI")

(quiet-load "cl-ppcre")
(quiet-load "local-time")

(let ((env (make-hash-table :test #'equal)))
  (mapcar (lambda (x) (setf (gethash x env) (sb-ext:posix-getenv x)))
	  '("DOCUMENT_ROOT"
	    "GATEWAY_INTERFACE"
	    "HTTP_ACCEPT"
	    "HTTP_ACCEPT_CHARSET"
	    "HTTP_ACCEPT_ENCODING"
	    "HTTP_ACCEPT_LANGUAGE"
	    "HTTP_CONNECTION"
	    "HTTP_COOKIE"
	    "HTTP_HOST"
	    "HTTP_KEEP_ALIVE"
	    "HTTP_REFERER"
	    "HTTP_USER_AGENT"
	    "PATH"
	    "QUERY_STRING"
	    "REMOTE_ADDR"
	    "REMOTE_PORT"
	    "REQUEST_METHOD"
	    "REQUEST_URI"
	    "SCRIPT_FILENAME"
	    "SCRIPT_NAME"
	    "SERVER_ADDR"
	    "SERVER_ADMIN"
	    "SERVER_NAME"
	    "SERVER_PORT"
	    "SERVER_PROTOCOL"
	    "SERVER_SIGNATURE"
	    "SERVER_SOFTWARE"))
  (defun getenv (x)
    (gethash x env)))

(defparameter *separators* '(#\& #\;))

(defparameter *table*		       ;by using an alist, I'm able to easily use this table for both decoding and encoding
  (list (cons #\Newline "%0A")
	(cons #\Return "%0D")
	(cons #\Space "+")
	(cons #\" "%22")
	(cons #\# "%23")
	(cons #\% "%25")
	(cons #\& "%26")
	(cons #\+ "%2B")
	(cons #\/ "%2F")
	(cons #\: "%3A")
	(cons #\; "%3B")
	(cons #\< "%3C")
	(cons #\= "%3D")
	(cons #\> "%3E")
	(cons #\? "%3F")
	(cons #\@ "%40")
	(cons #\[ "%5B")
	(cons #\\ "%5C")
	(cons #\] "%5D")
	(cons #\^ "%5E")
	(cons #\` "%60")
	(cons #\{ "%7B")
	(cons #\| "%7C")
	(cons #\} "%7D")
	(cons #\~ "%7E")))

(defun string-filter (s)
  (let ((pair (rassoc s *table* :test #'string=)))
    (if pair
	(format nil "~a" (car pair))
	s)))

(defun char-filter (c)
  (let ((pair (assoc c *table*)))
    (if pair
	(cdr pair)
	(format nil "~a" c))))

(defun join (by strings)
  (let ((glue (format nil "~a" by)))
    (reduce (lambda (x y) (concatenate 'string x glue y))
	    strings)))

(defun encode-token (s)
  (join "" (map 'list #'char-filter (coerce s 'list))))

(defun encode-url (alist)
  (join (first *separators*)
	(map 'list (lambda (pair) (format nil "~a=~a"
					  (encode-token (car pair))
					  (encode-token (cdr pair))))
	     alist)))

(defun decode-token (s)
  (labels ((iter (str res)
	     (cond
	       ((string= str "")
		res)
	       ((char= (char str 0) #\%)
		(iter (subseq str 3)
		      (cons (subseq str 0 3) res)))
	       (t
		(iter (subseq str 1)
		      (cons (format nil "~a" (char str 0)) res))))))
    (join "" (map 'list #'string-filter (nreverse (iter s nil))))))

(defun get-pair (s)
  (let ((pair (cl-ppcre:split "=" s)))
    (cons (decode-token (first pair))
	  (decode-token (second pair)))))
	  
(defun decode-url (s)
  (cond ((cl-ppcre:scan "&" s)
	 (map 'list #'get-pair (cl-ppcre:split "&" s)))
	((cl-ppcre:scan ";" s)
	 (map 'list #'get-pair (cl-ppcre:split ";" s)))))

(let ((query-string nil)
      (query-env (make-hash-table :test #'equal)))
  (defun get-query-string ()
    (unless query-string
      (setf query-string
	    (if (string= (getenv "REQUEST-METHOD") "POST")
		(read-line nil "")
		(getenv "QUERY-STRING"))))
    query-string)

  (loop for (name . value) in (decode-url (get-query-string))
       do (setf (gethash name query-env) value))
  
  (defun query (name)
    (gethash name query-env)))

(let ((cookies (make-hash-table)))
  (loop for (name . value) in (decode-url (getenv "HTTP_COOKIE"))
       do (setf (gethash name cookies) value))
  (defun cookie (name)
    (gethash name cookies)))

(defun unix-now ()
  (local-time:timestamp-to-unix (local-time:now)))

(defun unix-to-rfc-1123 (time)
  (local-time:format-timestring nil (local-time:unix-to-timestamp time) :format local-time:+rfc-1123-format+))

(defun set-cookie (name value &key (expiry nil))
  (format t "Set-Cookie: ~a" (encode-url (acons name value nil)))
  (when expiry
    (format t "; ~a" (unix-to-rfc-1123 (+ (unix-now) expiry))))
  (format t "~%"))
