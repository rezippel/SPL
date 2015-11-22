;;; -*- mode:lisp; package: WEYLI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;			 Weyli-extenstions for the Linear
;;;                           Algebra Transformations
;;; ===========================================================================
;;; (c) copyright 1995 Cornell University
(in-package 'weyli)

;; canonicalize

;; Produces a canonical form for inequalities.  This form
;; has a right-hand-side of 0 and a single sum of product
;; terms on the left.
(defmethod canonicalize ((ineq ge-eqn>))
  (eqn> (distributing-minus (lhs-of ineq) (rhs-of ineq)) 0))

(defmethod canonicalize ((ineq ge-eqn>=))
  (eqn>= (distributing-minus (lhs-of ineq) (rhs-of ineq)) 0))

(defmethod canonicalize ((ineq ge-eqn=))
  (eqn= (distributing-minus (lhs-of ineq) (rhs-of ineq)) 0))

;; lin-coefficient-of

;; Returns of the coefficient of a variable in an expression.
;; The expression must be linear in all variables. 
(defmethod lin-coefficient-of ((n numeric) (var ge-variable)) 0)

(defmethod lin-coefficient-of ((v ge-variable) (var ge-variable))
  (if (ge-same-var var v)
      1
      0))

(defmethod lin-coefficient-of ((p ge-plus) (var ge-variable))
  (let ((res 0))
    (loop for term in (terms-of p) do
      (setq res (+ res (lin-coefficient-of term var))))
    res))

(defmethod lin-coefficient-of ((p ge-times) (var ge-variable))
  (let ((terms (terms-of p)))
    (cond ((> (length terms) 2)
	   (error "Non-linear term in call to lin-coefficient-of."))
	  ((and (number? (first terms))
	        (ge-same-var var (second terms)))
	   (first terms))
	  ((and (number? (second terms))
	        (ge-same-var var (first terms)))
	   (second terms))
	  (t 0))))

(defmethod lin-coefficient-of ((g ge-or-numeric) (var ge-variable))
  (error "Illegal term in call to lin-coefficient-of."))

;; linear

;; A predicate which returns true if a given expression is linear
;; in all variables.
(defmethod linear ((var ge-variable)) t)

(defmethod linear ((n numeric)) t)

(defmethod linear ((g general-expression)) nil)

(defmethod linear ((p ge-times))
  (let ((terms (terms-of p)))
    (if (> (length terms) 2)
	nil
	(or (and (number? (first terms))
		 (linear (second terms)))
	    (and (number? (second terms))
		 (linear (first terms)))))))

(defmethod linear ((p ge-plus))
  (loop for f in (terms-of p)
	always (linear f)))

(defmethod linear ((equation ge-equation))
  (and (linear (rhs-of equation))
       (linear (lhs-of equation))))

;; distributing-minus

;; An special version of minus which produces a single ge-plus
;; when the second operand is a ge-plus.
(defmethod distributing-minus ((g ge-or-numeric) (p ge-plus))
  (let ((res g))
    (loop for term in (terms-of p) do
      (setq res (+ res (* -1 term))))
    res))

(defmethod distributing-minus ((g ge-or-numeric) (p ge-or-numeric))
  (- g p))

;; interval-from-ineq

;; When given a linear inequality and a variable present in that
;; inequality, returns an integer-sequence which defines the bounds
;; of that variable under the inequality.
;; e.g., (interval-from-ineq (eqn> i (+ j 2)) j) produces [-inf:i-3]
(defmethod interval-from-ineq ((ineq ge-eqn>) (var ge-variable))
  (if (not (linear ineq))
      nil
      (let ((i (canonicalize ineq)))
	(let ((c (lin-coefficient-of (lhs-of i) var)))
	  (cond ((> c 0)
	         (values (make-integer-sequence
			   (+ 1 (distributing-minus (* c var) (lhs-of i)))
			   *positive-infinity*)
		         c))
		((< c 0)
	         (values (make-integer-sequence
			   *negative-infinity*
			   (- (- (lhs-of i) (* c var)) 1))
		         (abs c)))
		(t nil))))))

(defmethod interval-from-ineq ((ineq ge-eqn>=) (var ge-variable))
  (if (not (linear ineq))
      nil
      (let ((i (canonicalize ineq)))
        (let ((c (lin-coefficient-of (lhs-of i) var)))
          (cond ((> c 0)
                 (values (make-integer-sequence
			   (distributing-minus (* c var) (lhs-of i))
                           *positive-infinity*)
                         c))
                ((< c 0)
                 (values (make-integer-sequence
			   *negative-infinity*
			   (- (lhs-of i) (* c var)))
                         (abs c)))
                (t nil))))))

;; testineq

;; Return the appropriate truth value of an inequality when both
;; sides are numbers, otherwise returns the inequality
(defmethod testineq ((ineq ge-eqn>))
  (if (and (number? (lhs-of ineq))
	   (number? (rhs-of ineq)))
      (> (lhs-of ineq)
	 (rhs-of ineq))
      ineq))

(defmethod testineq ((ineq ge-eqn>=))
  (if (and (number? (lhs-of ineq))
	   (number? (rhs-of ineq)))
      (>= (lhs-of ineq)
	 (rhs-of ineq))
      ineq))

(defmethod testineq ((ineq ge-eqn=))
  (if (and (number? (lhs-of ineq))
	   (number? (rhs-of ineq)))
      (= (lhs-of ineq)
	 (rhs-of ineq))
      ineq))
		

			  
        
