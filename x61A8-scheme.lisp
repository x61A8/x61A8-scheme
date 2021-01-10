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

(defun last1 (list)
  "Return the last element (not cons cell) of the list."
  (first (last list)))

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

;;; Evaluation
(defstruct scheme-primitive
  "Represents a scheme primitive."
  type)

(defstruct scheme-proc
  "Represents a scheme procedure."
  code (env nil) (params nil))

(defun scheval (exp env)
  "Evaluate the scheme expression in the scheme environment."
  (cond ((symbolp exp) (get-var exp env))
	((atom exp) exp)
	(t ; exp is a list
	 (let ((proc (scheval (first exp) env)))
	   (cond ((scheme-primitive-p proc)
		  (ecase (scheme-primitive-type proc)
		    (:quote (second exp))
		    (:lambda (make-scheme-proc :env env :params (second exp) :code (tag-list 'begin (rest2 exp))))
		    (:if (if (scheval (second exp) env)
			     (scheval (third exp) env)
			     (scheval (fourth exp) env)))
		    (:set! (set-var (second exp)
				    (scheval (third exp) env)
				    env))
		    (:begin (last1 (mapcar (lambda (sub-exp) (scheval sub-exp env))
					   (rest exp))))))
		 ((scheme-macro-intrinisic-p proc)
		  (scheval (call-scheme-macro (scheme-macro-intrinisic-name proc)
					      (rest exp))
			   env))
		 (t (let ((args (mapcar (lambda (arg) (scheval arg env)) (rest exp))))
		      (if (scheme-proc-p proc)			    
			  (scheval (scheme-proc-code proc) (extend-env (scheme-proc-params proc)
								       args
								       (scheme-proc-env proc)))
			  (apply proc args)))))))))

;;; Common Lisp function integration
(defparameter *cl-equivs*
  '(+ - * / = < > <= >= cons car cdr not append list read member
    (null? null) (eq? eq) (equal? equal) (eqv? eql)
    (write prin1) (display princ) (newline terpri))
  "Common Lisp functions which are equivalent to scheme functions, or trivial to convert.")

(defun init-cl-equiv (func)
  "Enter func into the scheme global environment."
  (if (symbolp func)
      (set-global-var func (symbol-function func))
      (if (symbolp (second func))
	  (set-global-var (first func) (symbol-function (second func)))
	  (set-global-var (first func) (compile nil (second func))))))

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

  (mapc #'init-cl-equiv *cl-equivs*)

  (loop for name being the hash-key of *registered-macros*
	do (set-global-var name (make-scheme-macro-intrinisic :name name)))
  
  *global-env*)

(defun start-scheme-rspl ()
  "Starts the read-scheval-print-loop."
  (loop (format t "~&=x> ")
	(print (scheval (read) nil))))

(defun scheme ()
  "Initialize and start the scheme rspl."
  (init-global-env)
  (start-scheme-rspl))

;;; Scheme Macros
(def-scheme-macro let (bindings &rest body)
  `((,(make-scheme-primitive :type :lambda) ,(mapcar #'first bindings) .,body)
    .,(mapcar #'second bindings)))
