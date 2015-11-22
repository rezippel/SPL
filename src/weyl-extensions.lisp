;;; -*- Mode: Lisp; Package: WEYL-INTERNALS -*-
;;; ===========================================================================
;;;		        Weyl Extensions for WRM
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University


(in-package "WEYLI")

;; We need to encapsulate code expressions inside Weyl expressions.
;; This is done by class

(defclass code-expression (general-expression ge-atom has-property-list)
  ((expression :initarg :expression
               :accessor expression-of)))

(defmethod print-object ((x code-expression) stream)
  (format stream "{~S}" (expression-of x)))

;; This really requires an equal operations for terms, but we don't
;; have one yet.  Instead we just convert to S-expressions and use
;; equal there.

(defmethod-sd ge-deriv ((exp code-expression) (var code-expression))
  (cond ((equal (wrmi::convert-to-sexp (expression-of exp))
		(wrmi::convert-to-sexp (expression-of var)))
	 (make-element domain 1))
	(t (make-element domain 0))))

(defmethod-sd ge-equal ((exp1 code-expression) (exp2 code-expression))
  (equal (expression-of exp1) (expression-of exp2)))

;; The following methods solve a single polynomial equation for its
;; most main variable 
(defmethod solve-if-possible ((pol epolynomial)) 
  (solve-if-possible (make-polynomial (domain-of pol) pol)))

;; If the argument is a linear polynomial, this routine returns the 
;; solution of the polynomial and the main variable of the polynomial.  If the
;; argument is not linear, then nil is returned. 
(defmethod solve-if-possible ((pol mpolynomial))
  (let ((form (poly-form pol))
        (cfield (coefficient-domain-of (domain-of pol)))
        terms)
    (cond ((poly-coef? form)
           nil)
          ((and (e1? (le (setq terms (poly-terms form))))
		(poly-coef? (lc terms)))
           (values 
            (if (terms0? (red terms))
              (coerce 0 cfield)
              (make-polynomial (domain-of pol)
                               (poly-times (poly-minus (lc (red terms))) 
                                           (/ (lc terms)))))
            (make-polynomial (domain-of pol)
                             (cons (poly-order-number form)
                                   (make-terms (e1) (one cfield))))))
          (t nil))))

(defmacro wrmi::possibly-simplify (expr)
  (declare (special wrmi::*walk-form-simplify-expressions-p*))
  `(setq ,expr
	 (if (and wrmi::*walk-form-simplify-expressions-p*
		  (typep ,expr 'general-expression))
	     (simplify ,expr)
	     ,expr)))

;; Local error approximations are extensions to equatins that are used
;; to describe the error properties of discretization techniques.  

(defclass local-error-approximation (ge-equation)
  ((step :initarg :step
	 :accessor step-of)
   (order :initarg :order
	  :accessor order-of)))

(defun make-local-error-approximation (lhs rhs step order)
  (unless (typep lhs 'domain-element)
    (setq lhs (coerce lhs *general*)))
  (unless (typep rhs 'domain-element)
    (setq rhs (coerce rhs *general*)))
  (make-instance 'local-error-approximation :domain (domain-of lhs)
		 :lhs lhs :rhs rhs :step step :order order))

(defmethod print-object ((obj local-error-approximation) stream)
  (format stream "~S =LE= ~S + O(~S^~D)"
	  (lhs-of obj) (rhs-of obj) (step-of obj) (order-of obj)))

;; Need to provide a WALKER and a SUBSTITUTE because of the extra slot
;; in a local-error over a regular equation.
(defmethod wrmi::walk-expression ((form local-error-approximation) context env)
  (let ((new-lhs (wrmi::walk-form-internal (lhs-of form) context env))
	(new-rhs (wrmi::walk-form-internal (rhs-of form) context env)))
    (unless (and (eql new-lhs (lhs-of form))
		 (eql new-rhs (rhs-of form)))
      (setq form (make-instance (class-of form)
				:domain (domain-of form)
				:lhs new-lhs :rhs new-rhs
				:order (order-of form)
				:step (step-of form)))
      (wrmi::possibly-simplify form))
    form))



(defmethod substitute ((value general-expression) (var ge-variable)
		       (expr local-error-approximation) &rest ignore)
  (declare (ignore ignore))
  (make-instance 'local-error-approximation
		 :domain (domain-of expr)
		 :lhs (substitute value var (lhs-of expr))
		 :rhs (substitute value var (rhs-of expr))
		 :order (order-of expr)
		 :step (step-of expr)))
