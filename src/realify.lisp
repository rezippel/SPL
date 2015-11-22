;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;			      Realify Transformer
;;; ===========================================================================
;;; (c) copyright 1993 Cornell University

(in-package "WRMI")

;; This package should take a program that include declarations of
;; variables as being complex, and transforms it into a program that
;; just uses regular floating point numbers.  Further declarations can
;; then take care of removing any remaining consing.


(defmethod split-real-imag (form env)
  (declare (ignore env)) 
  (values (realpart form) (imagpart form)))

(defmethod split-real-image ((form number) env)
  (declare (ignore env))
  (values form (zero *general*)))

(defmethod spit-real-imag ((form complex-number) env)
  (declare (ignore env))
  (values (realpart form)
          (imagpart form)))

(defmethod split-real-imag ((form weyli::ge-variable) env)
  (let ((realify (env-lookup form :realify env))
	type)
    (cond (realify
	    (values (first (first realify))
		    (second (first realify))))
	  ((and (setq type (first (env-lookup form :type env)))
		(typep (domain-of type) 'weyli::complex-numbers))
	   (values (realpart form)
		   (imagpart form)))
	  (t (values form (zero *general*))))))

(defmethod split-real-imag ((form weyli::ge-plus) env)
  (let ((domain (domain-of form))
        real imag)
    (loop for term in (terms-of form)
          do (multiple-value-bind (r i) (split-real-imag term env)
               (push r real) (push i imag)))

    (values (weyli::simp-plus-terms domain real)
            (weyli::simp-plus-terms domain imag))))

(defmethod split-real-imag ((form weyli::ge-times) env)
  (let ((terms (terms-of form)))
    (multiple-value-bind (real imag) (split-real-imag (first terms) env)
      (loop for term in (rest terms)
            do (multiple-value-bind (r i) (split-real-imag term env)
                 ;; Notice the PSETQ!!!!
                 (psetq real (- (* real r) (* imag i))
                        imag (+ (* real i) (* imag r)))))
      (values real imag))))

(defmethod split-real-imag ((form weyli::ge-expt) env)
  (multiple-value-bind (rbase ibase) (split-real-imag (base-of form) env)
    (let ((expt (exponent-of form)))
      (cond ((= expt 2)
	     (values (- (* rbase rbase) (* ibase ibase))
		     (* 2 rbase ibase)))
	    (t form)))))

;; FIXTHIS: The following isn't right!  What if some of the bound
;; variables are complex?
(defmethod split-real-imag ((form weyli::universal-quantified-set) env)
  (make-instance 'weyli::universal-quantified-set
       :domain (domain-of form)
       :bound-vars (bound-vars-of form)
       :expressions (loop for expr in (exprs-of form)
			  append (multiple-value-bind (rex iex)
				     (split-real-imag expr env)
				   (list rex iex)))))

(defun get-root-function (fun)
  (weyli::make-function (domain-of fun) (name-of fun) (nargs-of fun)))

(defmethod split-real-imag ((form weyli::ge-application) env)
  (let ((realify (env-lookup (funct-of form) :realify env)))
    (cond (realify
	    (values (apply (first (first realify)) (args-of form))
		    (apply (second (first realify)) (args-of form))))
	  ((and (typep (funct-of form) 'weyli::ge-function-deriv)
		(setq realify (env-lookup (get-root-function (funct-of form))
					  :realify env)))
	   (values (apply
		     (weyli::make-function-deriv (first (first realify))
						 (derivs-of (funct-of form)))
		     (args-of form))
		   (apply
		     (weyli::make-function-deriv (second (first realify))
						 (derivs-of (funct-of form)))
		     (args-of form))))
	  (t (values form (zero *general*))))))


(defmethod split-real-imag ((form weyli::ge-equation) env)
  (multiple-value-bind (rlhs ilhs) (split-real-imag (lhs-of form) env)
    (multiple-value-bind (rrhs irhs) (split-real-imag (rhs-of form) env)
      (values (make-instance (class-of form)
			     :domain (domain-of form) :lhs rlhs :rhs rrhs)
	      (make-instance (class-of form)
			     :domain (domain-of form) :lhs ilhs :rhs irhs)))))

;; Use *bindings* to
(defun realify (program)
  (walk-form program #'realify-walker))

(defvar *realify-walkers* ())

(defun insert-realify-walker (context type func)
  (let ((assoc-list (assoc context *realify-walkers*)))
    (cond ((null assoc-list)
           (push (list context (list type func))
                 *realify-walkers*))
          (t (loop for pair in (rest assoc-list)
                   do (when (eql (first pair) type)
                        (setf (cl:second pair) func)
                        (return t))
                   finally (push (list type func) (cl:rest assoc-list)))))))

(defun find-realify-walker (form context)
  (let ((assoc-list (assoc context *realify-walkers*)))
    (when assoc-list
      (loop for (type func) in (rest assoc-list)
            do (when (typep form type) 
                 (return func))))))

(defun realify-walker (form context env)
  (let ((func (find-realify-walker form context)))
    (if func (funcall func form env)
        form)))

(defmacro define-realify-walker (type context args &body body)
  (let ((func-name (intern (format nil "REALIFY-~A-~A" context type))))
    `(progn (defun ,func-name ,args ,@body)
            (insert-realify-walker ,context ',type ',func-name)
	    ',func-name)))

;; Does this type represent the complexes?
(defun complex-num-type? (type)
  (and type (domain-of type)
       (typep (domain-of type) 'weyli::complex-numbers)))

;; Does this type represent a vector of complexes?
(defun vector-complex-num-type? (type)
  (and (typep type 'vector-type)
       (elt-type-of type) 
       (domain-of (elt-type-of type))
       (typep (domain-of (elt-type-of type)) 'weyli::complex-numbers)))

(defun complex-function-type? (type)
  (and type (domain-of type)
       (typep (domain-of type) 'function-space)
       (typep (funct-range-of (domain-of type)) 'weyli::complex-numbers)))

(defun complex-valued-vars (prog)
  (let ((forms nil))
    (walk-form prog
               #'(lambda (form context env)
                   (declare (ignore context env))
                   (cond ((and (typep form 'binding)
                               (complex-num-type? (type-of form)))
                          (push (var-of form) forms))
                         ((typep form 'program)
                          (loop for arg in (args-of form)
                                do (when (complex-num-type? (second arg))
                                     (push (first arg) forms)))))
                   form))
    forms))

(define-realify-walker program :eval (form env)
  (declare (ignore env))
  (let (new-arg args bindings var type)
    (loop for arg in (args-of form) do
      (cond ((listp arg)
	     (setq var (first arg))
	     (setq new-arg (make-variable (weyli::symbol-of var)))
	     (setq type (second arg))
	     (push (list new-arg type) args)
	     (cond ((or (complex-num-type? type)
			(vector-complex-num-type? type))
		    (push (make-binding var new-arg type)
			  bindings))))
	    (t (push arg args))))
    (if bindings
	(values 
	  (make-program (name-of form) (nreverse args)
			(make-bind (reverse bindings) (term-of form)))
	  :recurse)
	form)))

(define-realify-walker weyli::general-expression :eval (form env)
  (multiple-value-bind (real imag) (split-real-imag form env)
    (values (make-application 'values (list real imag))
            :done)))

(define-realify-walker binding :binding (form env)
  (let ((type (type-of form))
	(var (var-of form))
	new-type real rvalue imag ivalue)
    (cond ((null type) form)
	  ((or (complex-num-type? type)
	       (vector-complex-num-type? type))
	   (setq real (make-variable 
			(intern (format nil "~Ar" var))))
	   (setq imag (make-variable
			(intern (format nil "~Ai" var))))
	   (if (null (value-of form))
	       (setq rvalue nil ivalue nil)
	       (multiple-value-setq (rvalue ivalue)
		   (split-real-imag (value-of form) env)))
	   (push (list var :realify (list real imag)) *new-envs*)
	   (setq new-type (if (complex-num-type? type) (get-real-numbers)
			      (wrm::vector
				(length-of type)
				(wrm::type (structure-of (elt-type-of type))
					   (get-real-numbers)))))
	   (list (make-binding real rvalue new-type)
		 (make-binding imag ivalue new-type)))
	  ((complex-function-type? type)
	   (setq real (weyli::make-function (domain-of var)
			      (intern (format nil "~Ar" (name-of var)))
			      (nargs-of var)))
	   (setq imag (weyli::make-function (domain-of var)
			      (intern (format nil "~Ai" (name-of var)))
			      (nargs-of var)))
	   (push (list var :realify (list real imag)) *new-envs*)
	   (setq new-type (wrm::differentiable-functions
			    (funct-domain-of (domain-of type))
			    (wrm::real-numbers)))
	   (list (make-binding real nil new-type)
		 (make-binding imag nil new-type)))
	  (t form))))

(define-realify-walker assignment :eval (form env)
  (multiple-value-bind (rloc iloc) (split-real-imag (location-of form) env)
    (multiple-value-bind (rval ival) (split-real-imag (value-of form) env)
      (values
       (cond ((0? iloc)
	      (unless (0? ival)
		      (error "Assigning a complex value to a real variable: ~%  ~S"
			     form))
	      form)
	     (t (make-sequence (list (make-assignment rloc rval)
				     (make-assignment iloc ival)))))
       :done))))

(define-realify-walker constrain :eval (form env)
  (values
    (make-constrain
      (loop for var in (vars-of form)
	    for ri-vars = (env-lookup var :realify env)
	    append (cond ((null ri-vars) (list var))
			  (t (first ri-vars))))
      (loop for form in (constraints-of form)
	    append (multiple-value-bind (rc ic)
		       (split-real-imag form env)
		       (if (null ic)
			   (list rc)
			 (list rc ic))))
      (term-of form))
    :recurse))
