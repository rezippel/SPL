;;; -*- mode:lisp; package: MLT; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;			      Complex Transformer
;;; ===========================================================================
;;; (c) copyright 1993 Cornell University

(in-package "MLT")

;; This package should take a program that include declarations of
;; variables as being complex, and transforms it into a program that
;; just uses regular floating point numbers.  Further declarations can
;; then take care of removing any remaining consing.

(defvar *bindings* nil)
(defvar *outer-let* nil)

(defmacro with-complex-arithmetic (&body body &environment env)
  (let ((*bindings* nil))
    (walk-form `(progn ,@body) env #'split-real-imag)))

#+IGNORE
(defun test ()
  (pprint
   (macroexpand-1
    '(with-complex-arithmetic
      (let ((x (complex 1 1)))
	(declare (type (complex double) x y))
	(setq x (+ (* x y) y))))))
  (values))

#+IGNORE
(defun foo ()
  (let ((x (complex 1 2))
	(y (complex 3 4))) 
    (declare (type (complex double) x y))
    (setq x (* a x))))

(defmacro define-splitter (op (form env) &body body)
  (let ((op-name (intern (format nil "SPLITTER-~A" op))))
    `(progn
       (defun ,op-name (,form ,env) ,@body)
       (setf (get ',op 'splitter) ',op-name))))

(defun complex-var? (sym env)
  (let ((decl (variable-declaration 'type sym env)))
    (and decl (listp (third decl))
	 (eql (first (third decl)) 'complex))))

(defun split-real-imag (form context env) 
  (cond ((eql context :eval)
	 (cond ((numberp form)
		form)
	       ((atom form)
		(if (complex-var? form env)
		    (values `(complex ,(symbol-realpart form env)
				      ,(symbol-imagpart form env))
			    t)
		    form))
	       ((get (first form) 'splitter)
		(funcall (get (first form) 'splitter) form env))
	       (t form)))
	((eql context :set)
	 (cond ((atom form)
		(let ((decl (variable-declaration 'type form env)))
		  (if (and decl (listp (third decl))
			   (eql (first (third  decl)) 'complex))
		      (values `(values ,(symbol-realpart form env)
				,(symbol-imagpart form env)))
		      form)))
	       (t form)))))

(define-splitter setq (form env)
  (let ((bind (nested-walk-form form (second form) env #'split-real-imag))
	(value (nested-walk-form form (third form) env #'split-real-imag)))
    (cond ((and (not (atom bind))
		(eql (first bind) 'complex))
	   (values `(progn
		     (setq ,(second bind) ,(split-realpart value env))
		     (setq ,(third bind) ,(split-imagpart value env)))
		   t))
	  (t (values `(setq ,bind ,value) t)))))

(define-splitter let (form env)
  (cond ((eql form *outer-let*)
	 form)
	(t (let* ((*outer-let* form)
		  (trans (nested-walk-form form form env #'split-real-imag))
		  decls bindings)
	     (loop for (var value) in (second trans)
		   for bind = (assoc var *bindings*)
		   do (cond (bind
			     (push `(,(second bind)
				     ,(split-realpart value env))
				   bindings)
			     (push `(,(third bind)
				     ,(split-imagpart value env))
				   bindings)
			     (when (fourth bind)
			       (push `(type ,(fourth bind)
				       ,(second bind) ,(third bind))
				     decls))
			     (setq *bindings* (delete bind *bindings*)))
			    (t (push (list var value) bindings)))
		      #+ignore
		      (unless (walker::variable-lexical-p (first bind) env)
			(format t "Extra binding: ~S" bind)))
	     (values `(let ,(reverse bindings)
		       (declare ,@decls)
		       ,@(rest (rest trans))) t)))))

(define-splitter the (form env)
  (declare (ignore env))
  (let ((type (second form))
	subtype walk-no-more?)
    (when (and (consp type)
	       (eql (first type) 'complex))
      (setq subtype (second type))
      (setq type (first type)))
    (cond ((eql type 'complex)
	   (if (atom (third form))
	       (setq walk-no-more? t))
	   (setq form
		 (cond ((null subtype)
			`(complex (realpart ,(third form))
				  (imagpart ,(third form))))
		       (t `(complex
			    (the ,subtype (realpart ,(third form)))
			    (the ,subtype (imagpart ,(third form)))))))))
    (values form walk-no-more?)))

(defun split-realpart (form env)
  (declare (ignore env))
  (cond ((atom form)
	 (if (complex-var? form env)
	     `(realpart ,form)
	     form))	 
	((eql (first form) 'complex)
	 (second form))
	(t `(realpart ,form))))

(defun split-imagpart (form env)
  (declare (ignore env))
  (cond ((atom form)
	 (if (complex-var? form env)
	     `(imagpart ,form)
	     0))
	((eql (first form) 'complex)
	 (third form))
	(t `(imagpart ,form))))

(defun split-symbol-realimag (form env)
  (let ((bind (assoc form *bindings*))
	(decl (variable-declaration 'type form env)))
    (cond ((and (not (atom (third decl)))
		(eql (first (third decl)) 'complex))
	   (setq decl (second (third decl))))
	  (t (setq decl nil)))
    (unless bind
      (setq bind (list form
		       (intern (format nil "~A-R" (symbol-name form))
			       (symbol-package form))
		       (intern (format nil "~A-I" (symbol-name form))
			       (symbol-package form))
		       decl))
      (push bind *bindings*))
    bind))

(defun symbol-realpart (form env)
  (second (split-symbol-realimag form env)))

(defun symbol-imagpart (form env)
  (third (split-symbol-realimag form env)))

(define-splitter realpart (form env)
  (cond ((atom (second form))
	 (symbol-realpart (second form) env))
	(t (values form t))))

(define-splitter imagpart (form env)
  (cond ((atom (second form))
	 (symbol-imagpart (second form) env))
	(t (values form t))))

(defun merge-sum (expr sum)
  (cond ((and (not (atom expr))
	      (eql (first expr) '+))
	 (append (rest expr) sum))
	((eql expr 0) sum)
	(t (cons expr sum))))

(defun simp-sum (sum)
  (if (rest sum) `(+ ,@sum)
      (first sum)))

(define-splitter + (form env)
  (let* ((complex? nil)
	 args reals imags new-arg
	 (forms (rest form)))
    (loop while forms do
      (setq new-arg (nested-walk-form form (pop forms) env #'split-real-imag))
      (setq args (merge-sum new-arg args))
      (when (not (atom new-arg))
	(when (or (eql (first new-arg) 'complex)
		  (and (eql (first new-arg) 'the)
		       (listp (second new-arg))
		       (eql (first (second new-arg)) `complex)))
	  (setq complex? t))))
    (setq args (nreverse args))
    (cond (complex? 
	   (loop for arg in args
		 do (setq reals (merge-sum (split-realpart arg env) reals))
		    (setq imags (merge-sum (split-imagpart arg env) imags)))
	   (setq form
		 `(complex ,(simp-sum (reverse reals))
			   ,(simp-sum (reverse imags)))))
	  (t (setq form  `(+ ,@args))))
    (values form t)))

(define-splitter * (form env)
  (let* ((complex? nil)
	 (args (loop for arg in (rest form)
		     for new-arg =
				 (nested-walk-form form arg env
					#'split-real-imag)
		     do (when (and (not (atom new-arg))
				   (eql (first new-arg) 'complex))
			  (setq complex? t))
		     collect new-arg))
	 real imag walk-no-more?)
    (when complex?
      (setq real (split-realpart (first args) env))
      (setq imag (split-imagpart (first args) env))

      (loop for arg in (rest args)
	    for real-arg = (split-realpart arg env)
	    and imag-arg = (split-imagpart arg env)
	    do (psetq real `(- (* ,real ,real-arg)
			       (* ,imag ,imag-arg))
		      imag `(+ (* ,real ,imag-arg)
			       (* ,imag ,real-arg))))
      (setq form `(complex ,real ,imag)))
    (values form walk-no-more?)))

(defmacro define-ilisp-transform (name (form) &body body)
  `(defun ,name (expression package)
     (ilisp::ilisp-errors
      (let ((*print-length* nil)
	    (*print-level* nil)
	    (*package* (ilisp::ilisp-find-package package)))
	(flet ((transform (,form) ,@body))
	  (pprint (transform (read-from-string expression))))))))

(define-ilisp-transform spl-realify (form) 
  (let ((*bindings* nil))
    (walk-form form nil #'split-real-imag)))

(define-ilisp-transform spl-macroexpand (form)
  (macroexpand-1 form))
