;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		      Algebraic Transformations
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University

;;; $Id: alg-trans.lisp,v 1.6 1994/11/18 22:25:51 rz Exp $

(in-package "WRMI")

;; Solve-LinearConstraints finds completely determined systems of
;; linear constraints and solves them exactly.

(defun solve-LinearConstraints (prog)
  (walk-form (mark-constrained-vars prog)
    #'(lambda (form context env)
        (if (and (eql context :eval) (touch? form))
	  (values 
           (multiple-value-bind (constraints exprs)
		                (apply #'env-constraints env (args-of form)) 
             (let ((code (and constraints
                              (triangularize-eqns constraints exprs))))
               (if (null code) form
                   (make-sequence1 code))))
           :recurse)
	  form))))

;; First argument is a set of linear equations in vars.  Result is a
;; set of triangularized linear equations (in general expression form).
(defun triangularize-eqns (constraints vars)
  (multiple-value-bind (eqs ring undone-kernels)
      (convert-to-polynomials constraints vars)
    (let ((gb (apply #'make-ideal ring eqs))
	  forms)
      (reduce-basis gb) 
      (setq forms 
	    (loop for eq in (generators-of gb)
		  collect
		  (multiple-value-bind (val var) (weyli::solve-if-possible eq)
                    ;; If value is nil, then the equation wasn't linear.  
                    ;; Back out completely.
                    (when (null val)
                      (return-from triangularize-eqns nil))
		    (make-assignment (coerce var *general*) 
				     (coerce val *general*)))))
      (if undone-kernels
	  (cons (make-touch undone-kernels) forms)
	  forms))))

;; Takes a set of expressions and variables and returns a list of
;; polynomials that are elements of the ring F[vars], where F is a
;; rational-function field over Q containing the kernels that appear
;; in expressions but which are not in vars.
(defun convert-to-polynomials
    (constraints vars &optional (scalars (get-rational-integers)))
  (let ((kernels (different-kernels constraints nil))
	ring)
    (loop for v in vars
	  do (when (member v kernels :test #'ge-equal)
	       (setq kernels (delete v kernels :test #'ge-equal))))
    ;; The remaining elements of kernels are not to be touched. 
    (setq ring (get-polynomial-ring
		 (get-quotient-field
		   (get-polynomial-ring scalars kernels))
		 vars))
    (values 
      (loop for c in constraints
	    collect (coerce (if (typep c 'weyli::ge-eqn=)
				(- (lhs-of c) (rhs-of c))
				c)
			    ring))
      ring
      kernels)))


;; The following transformation uses the lexical variable ordering and
;; Grobner bases to triangularize the algebraic constraints in a
;; constrain statement.
(defun solve-triangularize-walker (form context env)
  (declare (ignore env))
  (if (and (eql context :eval)
           (typep form 'constrain))
    (let* ((vars (vars-of form))
           constraints set-constraints
           domain
           gb)
      (loop for c in (constraints-of form)
            do (if (typep c 'set)
                 (push c set-constraints)
                 (push (if (typep c 'weyli::ge-eqn=)
                         (- (rhs-of c) (lhs-of c))
                         c)
                       constraints)))
      (cond ((null constraints)
             (values form :recurse))
            (t (setq domain (domain-of (first constraints)))
               (multiple-value-bind (eqs ring undone-vars)
                                    (convert-to-polynomials constraints vars)
                 (declare (ignore undone-vars))
                 (setq gb (apply #'make-ideal ring eqs))
                 (setf (greater-function gb) :lexical)      
                 (reduce-basis gb)
                 (values (make-constrain vars
                               (append set-constraints
                                       (loop for eq in (generators-of gb)
                                             collect (coerce eq domain)))
                            (term-of form))
                         :recurse)))))
    form))

(defun Solve-Triangularize (prog)
  (walk-form prog #'solve-triangularize-walker))

;; Extremalization routines

(defvar *lambda-counter* 0)

(defun resolve-maximize (prog)
  (walk-form (mark-constrained-vars prog '(:constrain :maximize))
    #'(lambda (form context env)
        (if (and (eql context :eval) (touch? form))
	  (values 
           (multiple-value-bind (constraints exprs)
		                (apply #'env-constraints env (args-of form))
             (multiple-value-bind (expressions vars)
                                  (apply #'env-extremize env :maximize (args-of form))
               (let ((lambdas (mapcar #'(lambda (x)
                                          (declare (ignore x))
                                          (make-variable 
                                           (format nil "Lam~D" (incf *lambda-counter*))))
                                      constraints))
                     sum)
                 (setq vars (union exprs vars :test #'ge-equal))
                 (setq sum (pop expressions))
                 (loop for exp in expressions 
                       do (setq sum (+ sum exp)))
                 (loop for l in lambdas
                       for e in constraints
                       do (cond ((ge-eqn=? e)
                                 (setq sum (+ sum (* l (- (lhs-of e) 
                                                          (rhs-of e))))))
                                ((not (typep e 'weyli::ge-equation))
                                 (setq sum (+ sum (* l e))))
                                (t (error "Inequality encountered!"))))
                 (make-constrain (append vars lambdas)
                                 (loop for v in vars
                                       collect (eqn= (deriv sum v) 0))
                                 (make-touch vars)))))
           :done)
	  form))))
