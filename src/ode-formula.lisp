;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;			ODE Discretization Formulas
;;; ===========================================================================
;;; (c) copyright 1993 Cornell University

;;; $id: upolynomial.lisp,v 2.11 1992/05/15 15:55:08 rz exp $

(in-package "WRMI")


;; General purpose tools.  These tools should be moved to weyl-extensions at
;; some point.

;; This performs the following types of transformations:
;; a*(b+c)  => ab + bc
;; but NOT
;;  (a+b)*(b+c) => ab + ac + b^2 + bc
(defun partial-expand-sum (expr)
  (let ((terms nil))
    (flet ((maybe-expand-product (prod)	     
	     (let ((prod-terms nil)
		   (sum-term nil))
	       (loop for fact in (terms-of prod) do
		 (cond ((ge-plus? fact)
			;; Two or more sums in product
			(when sum-term 
			  (setq sum-term nil)
			  (return nil))
			(setq sum-term fact))
		       (t (push fact prod-terms))))
	       (cond ((null sum-term)
		      (push prod terms))
		     (t (loop for addend in (terms-of sum-term) do
		       (push (weyli::simp-times-terms (domain-of expr)
						      (cons addend prod-terms))
			     terms)))))))
      (cond ((ge-plus? expr)
	     (loop for term in (terms-of expr) do
	       (if (not (ge-times? term)) (push term terms)
		   (maybe-expand-product term)))
	     (weyli::simp-plus-terms (domain-of expr) terms))
	    ((ge-times? expr)
	     (maybe-expand-product expr)	    
	     (weyli::simp-plus-terms (domain-of expr) terms))
	    (t expr)))))
			    
;; PARSE-ODE takes an equation or simple expression and a function,
;; and returns four values,
;;  (1) The terms that are linear in var
;;  (2) The non-linear terms that do not involve derivatives of var
;;  (3) The terms that involve first derivatives linearly
;;  (4) A list of other terms

(defmethod parse-ode ((eqn weyli::numeric) &rest vars)
  (declare (ignore vars))
  (let ((zero (zero (domain-of eqn))))
    (values eqn zero zero zero)))

(defmethod parse-ode ((eqn weyli::general-expression) &rest vars)
  (when (ge-eqn=? eqn)
    (setq eqn (- (lhs-of eqn) (rhs-of eqn))))
  (setq eqn (partial-expand-sum eqn))
  ;; At this point we have  sum of terms
  (let* ((zero (zero (domain-of eqn)))
	 (linear-terms zero)
	 (non-linear-terms zero)
	 (first-deriv zero)
	 (higher-derivs zero))
    (labels ((select-term (term)
	       (multiple-value-bind (deg ord) (apply #'term-order term vars)
		 (cond ((zerop ord)
			(if (< deg 2)
			    (setq linear-terms (+ term linear-terms))
			    (setq non-linear-terms (+ term non-linear-terms))))
		       ((and (= ord 1) (= deg 1))
			(setq first-deriv (+ term first-deriv)))
		       (t (setq higher-derivs (+ term higher-derivs)))))))
      (if (ge-plus? eqn)
	  (loop for term in (terms-of eqn)
		do (select-term term))
	  (select-term eqn)))
    (values linear-terms non-linear-terms first-deriv higher-derivs)))


;; All of the following routines return two values, the degree to
;; which the variable occurs and the order of the derivative if the
;; variable is a function.  If more than one argument is supplied, the
;; total degree and the maximum order are returned.

;; term-order  -- interprets the first argument as an expression
;; funct-order -- interprets the first argument as appearing in the
;;                functional position
(defmethod term-order ((term weyli::numeric) &rest vars)
  (declare (ignore vars))
  (values 0 0))

(defmethod term-order ((term weyli::ge-function) &rest vars)
  (values (if (member term vars) 1 0)
          0))

;; This only works for functions of a single argument
(defmethod term-order ((term weyli::ge-function-deriv) &rest vars)
  (if (member (weyli::name-of term) vars
	      :test #'(lambda (name fun)
			    (eql name (weyli::name-of fun))))
      (values 1 (length (derivs-of term)))
      (values 0 0)))

(defmethod term-order ((term weyli::ge-variable) &rest vars)
  (declare (ignore vars))
  (values 0 0))

(defmethod term-order ((term weyli::ge-plus) &rest vars)
  (let ((degree 0)
        (order 0))
    (loop for term in (terms-of term) do
          (multiple-value-bind (deg ord) (apply #'term-order term vars)
            (setq degree (max degree deg))
            (setq order (max order ord))))
    (values degree order)))

(defmethod term-order ((term weyli::ge-times) &rest vars)
  (let ((degree 0)
        (order 0))
    (loop for term in (terms-of term) do
          (multiple-value-bind (deg ord) (apply #'term-order term vars)
            (setq degree (+ degree deg))
            (setq order (max order ord))))
    (values degree order)))

(defmethod term-order ((term weyli::ge-expt) &rest vars)
  (if (number? (exponent-of term))
    (multiple-value-bind (deg ord) (apply #'term-order (base-of term) vars)
      (values (* deg (exponent-of term))
              (* ord (exponent-of term))))
    (values 100 0)))

(defmethod term-order ((term weyli::ge-application) &rest vars)
  (cond ((apply #'depends-on? (funct-of term) vars)
	 (cond ((apply #'depends-on? (args-of term) vars)
		(values 100 0))
	       ((ge-function-deriv? (funct-of term))
		(values 1 (length (derivs-of (funct-of term)))))
	       (t (values 1 0))))
	((apply #'depends-on? (args-of term) vars)
	 (values 100 0))
	(t (values 0 0))))

;; Divides EQN into two parts, those terms that are linear in the
;; elements of FUNCTS and those that are non-linear.
(defun split-linear (eqn functs)
  (multiple-value-bind (lin nlin first higher) (apply #'parse-ode eqn functs)
    (values (+ lin first) (+ nlin higher))))

(defun discretized-functions (env)
  (loop for env-form in env
	when (eql (second env-form) :discrete-function)
	  collect (first env-form)))

(defun discretized-variables (env)
  (loop for env-form in env
	when (eql (second env-form) :discrete-variable)
	  collect (first env-form)))

;; EQN can be a single Weyl expression or a list of Weyl expression.
;; Descritize the occurences of fucntions in EQN that depend on using
;; IVAR.
(define-iv-transform forward-euler (eqn env)
    (:local-error-order 2
     :name "Forward Euler")
  (let ((functs (discretized-functions env)))
    (flet ((transform (expr)
	     (multiple-value-bind (lin nlin first higher)
		 (apply #'parse-ode expr functs)
	       ;; FIXTHIS: Here we should check to see if there are any
	       ;; occurences of functs in the non-linears.
	       (+ (iv-discretize (+ lin nlin) env 0)
		  (iv-discretize (+ first higher) env 0)))))
      (list (make-instance (class-of eqn) :domain (domain-of eqn)
			   :rhs (transform (rhs-of eqn))
			   :lhs (transform (lhs-of eqn)))))))

(define-iv-transform backward-euler (eqn env)
    (:local-error-order 2
     :name "Backward Euler")
  (let ((functs (discretized-functions env)))
    (flet ((transform (expr)
	     (multiple-value-bind (lin nlin first higher)
		 (apply #'parse-ode expr functs)
	       (+ (iv-discretize lin env 1)
		  (iv-discretize nlin env 1)
		  (iv-discretize first env 0)
		  (iv-discretize higher env 0)))))
      (list (make-instance (class-of eqn) :domain (domain-of eqn)
			   :rhs (transform (rhs-of eqn))
			   :lhs (transform (lhs-of eqn)))))))

(define-iv-transform crank-nicolson (eqn env)
    (:local-error-order 3
     :name "Crank-Nicolson")
  (let ((functs (discretized-functions env)))
    (flet ((transform (expr)
	     (multiple-value-bind (lin nlin first higher)
		 (apply #'parse-ode expr functs)
	       ;; You really shouldn't be using this method if there are
	       ;; non-linear terms.
	       (+ (/ (+ (iv-discretize (+ lin nlin) env 0)
			(iv-discretize (+ lin nlin) env 1))
		     2)
		  (iv-discretize (+ first higher) env 0)))))
      (list (make-instance (class-of eqn) :domain (domain-of eqn)
			   :rhs (transform (rhs-of eqn))
			   :lhs (transform (lhs-of eqn)))))))

(define-iv-transform Adams-Bashforth-2 (eqn env)
    (:name "Adams-Bashforth"
     :local-error-order 3)
  (let ((functs (discretized-functions env)))
    (flet ((transform (expr)
	     (multiple-value-bind (lin nlin first higher)
		 (apply #'parse-ode expr functs)
	       (+ (/ (- (* 3 (iv-discretize (+ lin nlin) env 0))
                             (iv-discretize (+ lin nlin) env -1))
                          2)
		  (iv-discretize (+ first higher) env 0)))))
      (list (make-instance (class-of eqn) :domain (domain-of eqn)
			   :rhs (transform (rhs-of eqn))
			   :lhs (transform (lhs-of eqn)))))))

;; Stepping formulas

(define-iv-stepper constant-step (mvar new-funcs constraints)
  (:instance-vars ((step-size :initform nil
                              :initarg :step-size
                              :accessor step-size-of))
   :method-only t)
  (let ((label 
         (make-label (intern (format nil "L~D" (incf *label-counter*))))))
    `(,label
      ,(make-conditional (eqn> (getf  mvar :macro-step) 
                               (cont-var-of mvar))
         (make-sequence
          ;; Within this sequence we are only interested in constraints
          ;; that hold for instants within a given time interval.
          `(,(make-assignment (first (steps-of mvar)) (step-size-of stepper))
            ,(make-constrain new-funcs constraints 
                             (make-touch new-funcs))
            ,@(advance-forms mvar)
            ,(make-goto (name-of label))))))))

(defmethod pretty-name-of ((stepper constant-step))
  (format nil "Constant Step: ~A"
          (if (null (step-size-of stepper)) 
            "??"
            (step-size-of stepper))))

(defmethod print-object ((stepper constant-step) stream)
  (format stream "#<IV ~A>" (pretty-name-of stepper)))

(defmethod validate-stepper ((stepper constant-step))
  (unless (step-size-of stepper)
    t))

(defvar *iv-constant-step* (make-instance 'constant-step))

(define-iv-stepper constant-step-0.1 ()
  (:stepper-class constant-step
   :init-args (:step-size 0.1)))

(define-iv-stepper constant-step-0.01 ()
  (:stepper-class constant-step
   :init-args (:step-size 0.01)))

(define-iv-stepper constant-step-0.001 ()
  (:stepper-class constant-step
   :init-args (:step-size 0.001)))


