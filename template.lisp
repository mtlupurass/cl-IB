;;;example document:
;;;
;;;(doctype 'html' 'PUBLIC' '-//W3C//DTD XHTML 1.0 Strict//EN' 'http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd')
;;;(html :xmlns 'http://www.w3.org/1999/xhtml'
;;;  (head
;;;    (# 'this is a comment')
;;;    (meta :http-equiv 'content-type' :content 'text/html; charset=utf-8')
;;;    (title 'foo'))
;;;  (body
;;;    (div :align 'center'
;;;      (p 'hello,' (br) 'world!'))))

(defpackage :ib-templates
  (:use :common-lisp)
  (:export
   :load-template-file
   :compile-template))
(in-package "IB-TEMPLATES")

(ql:quickload "cl-ppcre")

(defparameter *insert* "[(]var 'insert'[)]")
(defparameter *debug* nil)

(let ((templates (make-hash-table)))
  (defun get-template (name)
    (gethash name templates))

  (defun get-template-tree (name)
    (car (get-template name)))
  
  (defun get-template-string (name)
    (cdr (get-template name)))
  
  (defun make-empty-template (name)
    (setf (gethash name templates) (cons nil nil)))

  (defun set-template-tree (name val)
    (setf (car (gethash name templates)) val))

  (defun set-template-string (name val)
    (setf (cdr (gethash name templates)) val)))

(defun list-to-string (l)
  (make-array (length l) :element-type 'base-char :initial-contents (reverse l)))

(defun att-p (s &key (is-string nil))
  (if is-string
      (char= #\: (elt s 0))
      (and (listp s)
	   (eq (first s) 'ATT))))

(defun quoted-p (s)
  (char= #\' (elt s 0)))

(defun special-html-p (s)
  (find s '("br" "hr" "link" "img" "meta" "input") :test #'string=))

(defun tag-p (s)
  (and (listp s)
       (eq (first s) 'TAG)))

(defun comment-p (s)
  (and (tag-p s)
       (string= (second s) "#")))

(defun sanitise-thing (s)
  (string-trim '(#\') s))

(defun sort-tags (the-list)
  (list 'TAG
	(first the-list)
	(map 'list #'second (remove-if-not #'att-p (rest the-list)))
	(remove-if #'att-p (rest the-list))))

(defun join (by strings)
  (let ((by (format nil "~a" by)))
    (reduce (lambda (x y) (concatenate 'string x by y)) strings)))

(defun join-atts (atts)
  (format nil " ~a" (join #\space atts)))

(defun startswith (regex str)
  (cl-ppcre:scan (format nil "^~a" regex) str))

(defun possibly-eval (att env)
  (destructuring-bind (name val) (cl-ppcre:split "=" att :limit 2)
    (if (startswith "\"[(]var" val)
	(format nil "~a=\"~a\"" name (var-eval (subseq val 6 (- (length val) 2)) ;strip away the `"(var' and the `)"'
					       env))
	att)))
(defmacro sethash (obj key hash-table)
  `(setf (gethash ,key ,hash-table) ,obj))

(defun make-hash-aux (&rest pairs)
  (let ((res (make-hash-table)))
    (dolist (i pairs)
      (sethash (second i) (first i) res))
    res))

(defmacro make-hash (&rest pairs)
  `(make-hash-aux ,@(loop for i in pairs collecting `(list (quote ,(first i)) ,(second i)))))

(defun special-tag-p (tag)
  (find tag '("var" "loop" "if") :test #'string=))

(defun to-symbol (str)
  (intern (string-upcase str)))

(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(defmacro with-unpacked-alist (alist &body body)
  `(progv (map 'list #'car ,alist) (map 'list #'cdr ,alist)
     ,@body))

(defun hashtable-to-alist (hashtable)
  (loop for key being the hash-keys of hashtable
        using (hash-value value)
        collecting (cons key value)))

(defmacro with-unpacked-hashtable (hashtable &body body)
  `(with-unpacked-alist (hashtable-to-alist ,hashtable)
     ,@body))

(defun tag-eval (tag things env)
  (case (to-symbol tag)
    (VAR (var-eval (first things) env))
    (LOOP (loop-eval things env))
    (IF (if-eval things env))))

(defun var-eval (thing env)
  (handler-bind ((warning #'ignore-warning))
    (eval `(with-unpacked-hashtable ,env
	     ,(read-from-string thing)))))

(defun loop-eval (things env)
  (let ((var (var-eval (list (first things))
			     env)))
    (join ""
	  (loop for local-env in var collecting (tags-to-html (rest things) local-env)))))
    
(defun if-eval (things env)
  (when (var-eval (first things) env)
    (tags-to-html (rest things) env)))

(defun tokenise (s)
  (labels ((iter (chars s state res)
	     (when *debug*
	       (format t "~&------~%state: ~a~%char: ~@c~%s: ~a~%res: ~s~%"
		       state (first chars) (list-to-string s) (reverse res)))
	     (if (null chars)
		 res
		 (let ((char (first chars)))
		   (case state
		     (0
		      (cond ((char= #\( char)
			     (iter (rest chars) nil 1 (cons "(" res)))
			    ((char= char #\')
			     (iter (rest chars) (cons char s) 2 res))
			    ((find char '(#\space #\tab #\newline))
			     (iter (rest chars) s 0 res))
			    (t
			     (iter (rest chars) (cons char s) 1 res))))
		     (1
		      (cond ((find char '(#\space #\tab #\newline))
			     (iter (rest chars) nil 0 (cons (list-to-string s) res)))
			    ((char= #\) char)
			     (iter (rest chars) nil 3 (append (list ")" (list-to-string s)) res)))
			    (t
			     (iter (rest chars) (cons char s) 1 res))))
		     (2
		      (cond ((char= #\' char)
			     (iter (rest chars) (cons char s) 1 res))
			    (t
			     (iter (rest chars) (cons char s) 2 res))))
		     (3
		      (cond ((char= #\) char)
			     (iter (rest chars) nil 3 (cons ")" res)))
			    ((find char '(#\space #\newline))
			     (iter (rest chars) nil 0 res))
			    ((char= #\tab char)
			     (iter(rest chars) nil 3 res))
			    (t
			     (error "invalid expression")))))))))
    (nreverse (iter (coerce s 'list) nil 0 nil))))

(defun parse (tlist)
  (labels ((iter ()
	     (let ((token (pop tlist)))
	       (cond
		 ((null token)
		  nil)
		 ((string= token ")")
		  (error "unexpected ')'"))
		 ((string= token "(")
		  (let ((the-list (loop while (string/= (first tlist) ")")
				     collecting (iter))))
		    (pop tlist)
		    (sort-tags (remove-if #'comment-p the-list))))
		 ((att-p token :is-string t)
		  (list 'ATT (format nil "~a=\"~a\"" (subseq token 1) (sanitise-thing (pop tlist)))))
		 ((quoted-p token)
		  (sanitise-thing token))
		 (t token)))))
    (loop while tlist
	 collecting (iter))))

(defun to-html (tree env)
  (if (listp tree)
      (destructuring-bind (tag atts things) (rest tree)
	(cond ((special-tag-p tag)
	       (tag-eval tag things env))
	      ((string= tag "doctype")
	       (apply #'format nil "<!DOCTYPE ~a ~a \"~a\" \"~a\">" things))
	      ((special-html-p tag)
	       (format nil "<~a~a />" tag
		      (if atts (join-atts atts) "")))
	      (t
	       (format nil "<~a~a>~a</~a>"
		       tag
		       (if atts (join-atts (map 'list
						(lambda (x) (possibly-eval x env))
						atts))
			   "")
		       (if things (join "" (loop for thing in things collecting (to-html thing env))) "")
		       tag))))
      tree))

(defun tags-to-html (tags env)
  (join "" (loop for i in tags collecting (to-html i env))))

(defun compile-template-aux (template env)
  (tags-to-html (get-template-tree template) env))

(defmacro compile-template (template &body env-list)
  `(compile-template-aux ,template (make-hash ,@env-list)))

(defun read-lines (filename)
  (with-open-file (in filename :if-does-not-exist nil)
    (when in
      (loop for line = (read-line in nil)
	   while line
	   collecting line))))

(defun pt (s)
  (parse (tokenise s)))

(defun wrap-into (wrappers wrappee)
  (let ((res wrappee))
    (loop for i in wrappers
	 do (setf res (cl-ppcre:regex-replace *insert* (get-template-string i) res)))
    res))

(defun get-templates (the-list state tmp)
  (when the-list
    (case state
      (0
       (let ((regs (nth-value 1 (cl-ppcre:scan-to-strings "^CREATE-TEMPLATE (.+) ([(].*?[)])$" (first the-list)))))
	 (if regs
	     (get-templates (rest the-list) 1 (list (read-from-string (elt regs 1)) (to-symbol (elt regs 0))))
	     (get-templates (rest the-list) 0 tmp))))
      (1
       (cond ((cl-ppcre:scan "^END-TEMPLATE$" (first the-list))
	      (let* ((tlist (reverse tmp))
		     (name (car tlist))
		     (wrappers (cadr tlist))
		     (temp (join #\newline (cddr tlist))))
		(make-empty-template name)
		(if wrappers
		    (set-template-tree name (pt (wrap-into wrappers temp)))
		    (set-template-tree name (pt temp)))
		(set-template-string name temp))
	      (get-templates (rest the-list) 0 nil))
	     (t
	      (get-templates (rest the-list) 1 (cons (first the-list) tmp))))))))

(defun load-template-file (filename)
  (get-templates (read-lines filename) 0 nil))
