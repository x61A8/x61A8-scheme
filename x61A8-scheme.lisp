(defpackage #:x61A8-scheme
  (:nicknames #:x6s)
  (:use #:cl))

(in-package #:x61A8-scheme)

;;; Utilities
(defun tag-list (tag list)
  "If list does not start with tag, add tag."
  (if (eq tag (first list))
      list
      (cons tag list)))

(defun rest2 (list)
  "Applies rest to the list twice."
  (rest (rest list)))

;;; Environments
(defvar *global-env* nil "The global scheme environment.")

(defun get-global-var (var)
  "Get the value of var in the global scheme environment."
  (multiple-value-bind (val presentp) (gethash var *global-env*)
    (if presentp
	val
	(error "Reference of unbound variable ~S." var))))

(defun set-global-var (var val)
  "Set the value of var to val in the global scheme environment."
  (setf (gethash var *global-env*) val))

(defun get-var (var env)
  "Get the value of var in env, or if not found, in the global environment."
  (let ((var-val-pair (assoc var env)))
    (if var-val-pair
	(cdr var-val-pair)
	(get-global-var var))))

(defun set-var (var val env)
  "Set the value of var in env to val, or if not found, the global environment."
  (let ((var-val-pair (assoc var env)))
    (if var-val-pair
	(setf (cdr var-val-pair) val)
	(set-global-var var val))
    val))

(defun extend-env (vars vals env)
  "Add the var-val pairs to env."
  (nconc (mapcar #'cons vars vals) env))

;;; Scheme macro infrastructure
(defvar *registered-macros* (make-hash-table :test 'eq)
  "Storage of scheme macros (functions that return scheme expressions).")

(defstruct scheme-macro-intrinisic
  "Structure used to identify macros during schevaluation."
  name)

(defmacro def-scheme-macro (name params &body body)
  "Define a scheme macro and register it."
  `(setf (gethash ',name *registered-macros*)
	 (lambda ,params .,body)))

(defun call-scheme-macro (name args)
  "Find the macro name in *registered-macros* and call it with args."
  (apply (gethash name *registered-macros*) args))

(defun install-scheme-macro (name macro)
  "Enter the macro `name` into *global-env* as name."
  (declare (ignore macro))
  (set-global-var name (make-scheme-macro-intrinisic :name name)))

;;; Evaluation
(defstruct scheme-primitive
  "Represents a scheme primitive."
  type)

(defstruct scheme-proc
  "Represents a scheme procedure."
  code (env nil) (params nil))

(defun scheval (expression environment continuation)
  "Evaluate the scheme expression in the scheme environment, passing the result to continuation."
  (prog ()
   :scheval
     (let ((exp expression)
	   (env environment)
	   (cc continuation))
       (return
	 (cond ((symbolp exp) (funcall cc (get-var exp env)))
	       ((atom exp) (funcall cc exp))
	       (t ; exp is a list
		(setf expression (first exp)
		      continuation
		      (lambda (proc)
			(cond ((scheme-primitive-p proc)
			       (ecase (scheme-primitive-type proc)
				 (:quote
				  (funcall cc (second exp)))
				 (:lambda
				     (funcall cc (make-scheme-proc :env env
								   :params (second exp)
								   :code (tag-list 'begin (rest2 exp)))))
				 (:if
				  (setf expression (second exp)
					environment env
					continuation (lambda (pred)
						       (setf expression (if pred (third exp) (fourth exp))
							     environment env
							     continuation cc)
						       (go :scheval)))
				  (go :scheval))
				 (:set!
				  (setf expression (third exp)
					environment env
					continuation (lambda (value)
						       (funcall cc (set-var (second exp) value env))))
				  (go :scheval))
				 (:begin
				  (pop exp) ; remove 'begin
				  (labels ((scheval-begin (value)
					     (if (null exp)
						 (funcall cc value)
						 (progn
						   (setf expression (pop exp)
							 environment env
							 continuation #'scheval-begin)
						   (go :scheval)))))
				    (scheval-begin nil)))
				 (:call/cc
				  (setf expression (list (second exp) (lambda (cont val)
									(declare (ignore cont))
									(funcall cc val)))
					environment env
					continuation cc)
				  (go :scheval))))
			      ((scheme-macro-intrinisic-p proc)
			       (setf expression (call-scheme-macro (scheme-macro-intrinisic-name proc)
								   (rest exp))
				     environment env
				     continuation cc)
			       (go :scheval))
			      (t			       
			       (let ((accum-args nil))
				 (labels ((scheval-application (args)					  
					    (if (scheme-proc-p proc)
						(progn
						  (setf expression (scheme-proc-code proc)
							environment (extend-env (scheme-proc-params proc)
										args
										(scheme-proc-env proc))
							continuation cc)
						  (go :scheval))
						(apply proc cc args)))
					  (scheval-args (remaining-exps)
					    (if (null remaining-exps)
						(scheval-application (reverse accum-args))
						(progn
						  (setf expression (first remaining-exps)
							environment env
							continuation (lambda (val)
								       (push val accum-args)
								       (scheval-args (rest remaining-exps))))
						  (go :scheval)))))				   
				   (scheval-args (rest exp))))))))
		(go :scheval)))))))

;;; Common Lisp function integration
(defparameter *cl-equivs*
  '(;; Booleans
    not (boolean? (lambda (obj) (typep obj 'boolean)))

    ;; Equivalence predicates
    (eqv? eql) (eq? eq) (equal? equal)

    ;; Pairs and lists
    (pair? consp) cons car cdr
    (set-car! (lambda (pair obj) (setf (car pair) obj)))
    (set-cdr! (lambda (pair obj) (setf (cdr pair) obj)))
    CAAR CADR CDAR CDDR CAAAR CAADR CADAR CADDR CDAAR CDADR CDDAR CDDDR
    CAAAAR CAAADR CAADAR CAADDR CADAAR CADADR CADDAR CADDDR CDAAAR CDAADR
    CDADAR CDADDR CDDAAR CDDADR CDDDAR CDDDDR
    (null? null) list length append reverse
    (list-tail (lambda (list k) (nthcdr k list)))
    (list-ref (lambda (list k) (nth k list)))
    (last-pair last)
    (memq (lambda (obj list) (member obj list :test #'eq)))
    (memv (lambda (obj list) (member obj list :test #'eql)))
    (member (lambda (obj list) (member obj list :test #'equal)))
    (assq (lambda (obj alist) (assoc obj alist :test #'eq)))
    (assv (lambda (obj alist) (assoc obj alist :test #'eql)))
    (assoc (lambda (obj alist) (assoc obj alist :test #'equal)))

    ;; Symbols
    (symbol? symbolp) (symbol->string symbol-name) (string->symbol make-symbol)

    ;; Numbers
    (number? numberp) (complex? complexp) (real? realp) (rational? rationalp)
    (integer? integerp) (zero? zerop) (positive? plusp) (negative? minusp)
    (odd? oddp) (even? evenp)
    (exact? (lambda (z) (or (typep z 'rational)
			    (typep z '(complex (rational))))))
    (inexact? (lambda (z) (or (typep z 'float)
			      (typep z '(complex (float))))))
    = < > <= >= max min + * - / abs
    (quotient truncate) (remainder rem) (modulo mod)
    numerator denominator gcd lcm
    floor ceiling truncate round
    rationalize ; MISSING: two arg rationalize
    exp log sin cos tan asin acos atan sqrt expt
    (make-rectangular complex)
    (make-polar (lambda (magnitude angle) (complex (* (cos angle) magnitude)
						   (* (sin angle) magnitude))))
    (real-part realpart) (imag-part imagpart)
    (magnitude abs) (angle phase)
    (exact->inexact (lambda (z) (if (typep z 'complex)
				    (coerce z '(complex (float)))
				    (float z))))
    (inexact->exact (lambda (z) (if (typep z 'complex)
				    (coerce z '(complex (rational)))
				    (rational z))))
    ;; MISSING: number->string string->number

    ;; Characters
    (char? characterp) (char=? char=) (char<? char<) (char>? char>)
    (char<=? char<=) (char>=? char>=) (char-ci=? char-equal)
    (char-ci<? char-lessp) (char-ci>? char-greaterp)
    (char-ci<=? char-not-greaterp) (char-ci>=? char-not-lessp)
    (char-alphabetic? alpha-char-p) (char-numeric? digit-char-p)
    (char-whitespace? (lambda (char) (or (char= char #\Space)
					 (not (graphic-char-p char)))))
    (char-upper-case? upper-case-p) (char-lower-case? lower-case-p)
    (char->integer char-code) (integer->char code-char)
    char-upcase char-downcase

    ;; Strings
    (string? stringp)
    (make-string (lambda (k &optional (char #\Nul)) (make-string k :initial-element char)))
    (string-length length) (string-ref char)
    (string-set! (lambda (string k c) (setf (char string k) c)))
    (string=? string=) (string-ci=? string-equal)
    (string<? string<) (string>? string>) (string<=? string<=) (string>=? string>=) 
    (string-ci<? string-lessp) (string-ci>? string-greaterp)
    (string-ci<=? string-not-greaterp) (string-ci>=? string-not-lessp)
    (substring subseq) (string-append (lambda (&rest strings) (apply #'concatenate 'string strings)))
    (string->list (lambda (string) (coerce string 'list)))
    (list->string (lambda (list) (coerce list 'string)))
    (string-copy copy-seq) (string-fill! fill)

    ;; Vectors
    (vector? vectorp) (make-vector (lambda (k &optional (fill 0)) (make-array k :initial-element fill)))
    vector (vector-length length) (vector-ref aref)
    (vector-set! (lambda (vector k obj) (setf (aref vector k) obj)))
    (vector->list (lambda (vector) (coerce vector 'list)))
    (list->vector (lambda (list) (coerce list 'simple-vector)))
    (vector-fill! fill)

    ;; Control features
    (procedure? (lambda (obj) (or (scheme-proc-p obj) (functionp obj))))
    
    
    read (write prin1) (display princ) (newline terpri))
  "Common Lisp functions which are equivalent to scheme functions, or trivial to convert.")

(defun init-cl-equiv (func)
  "Modify func to accept a continuation and then enter func into the scheme global environment."
  (if (symbolp func)
      (init-cl-equiv (list func func))
      (set-global-var (first func)
		      (lambda (cc &rest args)
			(funcall cc (apply (if (symbolp (second func))
					       (symbol-function (second func))
					       (compile nil (second func)))
					   args))))))

;;; User Interaction
(defun init-global-env ()
  "Initializes the global scheme environment for initial use."
  (setf *global-env* (make-hash-table :test 'eq))

  (set-global-var 't t)
  (set-global-var 'nil nil)
  
  (set-global-var 'quote (make-scheme-primitive :type :quote))
  (set-global-var 'lambda (make-scheme-primitive :type :lambda))
  (set-global-var 'if (make-scheme-primitive :type :if))
  (set-global-var 'set! (make-scheme-primitive :type :set!))
  (set-global-var 'begin (make-scheme-primitive :type :begin))
  
  (set-global-var 'call-with-current-continuation (make-scheme-primitive :type :call/cc))
  (set-global-var 'call/cc (make-scheme-primitive :type :call/cc))
  
  (mapc #'init-cl-equiv *cl-equivs*)
  (maphash #'install-scheme-macro *registered-macros*) 
  *global-env*)

(defun start-scheme-rspl ()
  "Starts the read-scheval-print-loop."
  (loop (format t "~&=x> ")
	(finish-output)
	(scheval (read) nil #'print)))

(defun scheme ()
  "Initialize and start the scheme rspl."
  (init-global-env)
  (start-scheme-rspl))

;;; Scheme Macros
(def-scheme-macro let (bindings &rest body)
  `((,(make-scheme-primitive :type :lambda) ,(mapcar #'first bindings) .,body)
    .,(mapcar #'second bindings)))

(def-scheme-macro cond (&rest clauses)
  (let ((test (first (first clauses)))
	(sequence (rest (first clauses))))
    (cond ((null clauses) nil)
	  ((eq 'else test)
	   `(,(make-scheme-primitive :type :begin) . ,sequence))
	  ((null sequence)
	   `(,(make-scheme-macro-intrinisic :name 'or) ,test (,(make-scheme-macro-intrinisic :name 'cond) .,(rest clauses))))
	  (t `(,(make-scheme-primitive :type :if) ,test
	       (,(make-scheme-primitive :type :begin) .,sequence)
	       (,(make-scheme-macro-intrinisic :name 'cond) .,(rest clauses)))))))

(def-scheme-macro and (&rest tests)
  (cond ((null tests) t)
	((= (length tests) 1) (first tests))
	(t `(,(make-scheme-primitive :type :if) ,(first tests)
	     (,(make-scheme-macro-intrinisic :name 'and) .,(rest tests))
	     nil))))

(def-scheme-macro or (&rest tests)
  (cond ((null tests) nil)
	((= (length tests) 1) (first tests))
	(t (let ((var (gensym)))
	     `(,(make-scheme-macro-intrinisic :name 'let) ((,var ,(first tests)))
	       (,(make-scheme-primitive :type :if) ,var
		,var
		(,(make-scheme-macro-intrinisic :name 'or) .,(rest tests))))))))

(def-scheme-macro letrec (bindings &rest body)
  `(,(make-scheme-macro-intrinisic :name 'let) ,(mapcar (lambda (v) (list (first v) nil)) bindings)
    ,@(mapcar (lambda (v) `(,(make-scheme-primitive :type :set!) .,v)) bindings)
    .,body))

(def-scheme-macro define (name &rest body)
  (if (atom name)
      `(,(make-scheme-primitive :type :begin) (,(make-scheme-primitive :type :set!) ,name . ,body) ',name)
      `(,(make-scheme-macro-intrinisic :name 'define) ,(first name)
	     (lambda ,(rest name) . ,body))))
