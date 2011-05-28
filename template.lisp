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

(defparameter *debug* nil)

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

(defmacro sethash (obj key hash-table)
  `(setf (gethash ,key ,hash-table) ,obj))

(defun make-hash-aux (&rest pairs)
  (let ((res (make-hash-table)))
    (dolist (i pairs)
      (sethash (second i) (first i) res))
    res))

(defmacro make-hash (&rest pairs)
  `(make-hash-aux ,@(loop for i in pairs collecting `(list ,@i))))

(defun special-tag-p (tag)
  (find tag '("var" "loop" "const" "if") :test #'string=))

(defun to-symbol (str)
  (intern (string-upcase str)))

(defgeneric tag-eval (tag things env)
  (:documentation "evaluate a special tag. Special tags are: var, loop, const, and if."))

(defmethod tag-eval ((tag (eql "var")) things env)
  (multiple-value-bind (value exists-p) (gethash (to-symbol (first things)) env)
    (if exists-p
	value
	(error "no such variable in environment"))))

(defmethod tag-eval ((tag (eql "loop")) things env)
  ;evaluate loop in here
  )

(defmethod tag-eval ((tag (eql "const")) things env)
  ;evaluate const in here
  )

(defmethod tag-eval ((tag (eql "if")) things env )
  ;evaluate if in here
  )

(defmethod tag-eval (tag things env)
  (error (format nil "~a is not a special tag." tag)))

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

(defun to-html (tree)
  (if (listp tree)
      (destructuring-bind (tag atts things) (rest tree)
	(cond ((string= tag "doctype")
	       (apply #'format nil "<!DOCTYPE ~a ~a \"~a\" \"~a\">" things))
	      ((special-html-p tag)
	       (format nil "<~a~a />" tag
		      (if atts (join-atts atts) "")))
	      (t
	       (format nil "<~a~a>~a</~a>"
		       tag
		       (if atts (join-atts atts) "")
		       (if things (join "" (map 'list #'to-html things)) "")
		       tag))))
      tree))

(defun compile-template (s)
  (join "" (loop for i in (parse (tokenise s))) collecting (to-html i)))
