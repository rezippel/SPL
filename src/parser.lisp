;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		 General Parser for Input Expressions
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University

;;; $Id: parser.lisp,v 1.6 1995/07/27 17:06:16 mardis Exp $

(in-package "WRMI")

;;; The program will initially start out as an S-expression in the WRM
;;; package.  The code walker in this file will replace all of the
;;; occurences of expressions that could produce Weyl expressions into
;;; the the Weyl expressions themselves.

(defvar *bindings* ())

(defmacro defwrmvar (var value)
  `(setf (get ',var 'wrm-value) (list ,value)))

(defwrmvar wrm::*positive-infinity* *positive-infinity*)
(defwrmvar wrm::*negative-infinity* *negative-infinity*)

(defwrmvar wrm::nil nil)

;; Define the def-program-parser forms
(def-walker-form Parser)

(defvar *last-spl-function* nil)

(defmacro wrm::defprogram (name args &body body)
  `(setq *last-spl-function* 
         (setq ,(intern (string name) 'wrmi)
	       '(wrm::defprogram ,name ,args ,@body))))

(defvar *parser-label-counter* 0)

(defmacro new-parser-label ()
  `(intern (format nil "LP~D" (incf *parser-label-counter*))))

(defun parse-program (program)
  (let ((*bindings* ())
	(*variable-counter*  0)
        (*parser-label-counter* 0))
    (parse-program* program)))

(defun parse-program* (form)	
  (let ((walker (and (not (atom form))
		     (symbolp (first form))
		     (get (first form) 'program-parser)))
	property) 
    (cond ((atom form)
	   (cond ((or (null form) (not (symbolp form))) form)		 
                 ((setq property (get form 'wrm-value))
		  (first property))
                 ((loop for (var binding) in *bindings*
			do (when (eql var form)
			     (return (values binding t)))))
		 ((symbolp form) (coerce form *general*))
                 (t (make-term form))))
	  ((not (null walker))
	   (funcall walker form))
	  ((and (not (atom form)) (fboundp (first form)))
	   (apply (first form)
		  (mapcar #'parse-program* (rest form))))
	  (t (make-application (parse-program* (first form))
			       (mapcar #'parse-program* (rest form)))))))

;; Parses a program that will be used in an arithmetic expression
(defun parse-program-arith (form)
  (setq form (parse-program* form))
  (cond ((or (numberp form) (symbolp form))
	 (coerce form *general*))
	((typep form 'weyli::domain-element)
	 form)
	(t (make-instance 'weyli::code-expression
			  :domain *general*
			  :expression form))))

;; Its always necessary.  This is vestigial. 
(defun add-sequence-if-necessary (forms)
  (if (null (rest forms)) (first forms)
      (cons 'wrm::sequence forms)))

(defun parse-lambda-internal (arguments terms)
  (let ((*bindings* *bindings*)
	args)
    (loop for arg in arguments
	  with v
	  do (cond ((atom arg)
		    (setq v (coerce arg *general*))
		    (push (list arg v) *bindings*)
		    (push v args))
		   (t (setq v (coerce (first arg) *general*))
		      (push (list (first arg) v) *bindings*)
		      (push (list v (ensure-bi-type
				      (parse-program* (second arg))))
			    args))))
    (values (nreverse args)
	    (parse-program* (add-sequence-if-necessary terms)))))

(def-program-parser defprogram (form)
  (multiple-value-bind (args term)
      (parse-lambda-internal (third form) (nthcdr 3 form))
    (make-program (second form) args term)))

(def-program-parser lambda (form)
  (multiple-value-bind (args term)
      (parse-lambda-internal (second form) (nthcdr 2 form))
    (make-function args term)))
			    
(def-program-parser interval (form)
  (interval-parser+ 'interval form))

(def-program-parser periodic-interval (form)
  (interval-parser+ 'periodic-interval form))  

(defun interval-parser+ (interval-class form)
  (let ((start (second form))
	(start-type :closed)
	(finish (third form)) 
	(finish-type :closed)
	step-size ambient interval)
    (unless (atom start)
      (setq start-type (second start))
      (setq start (first start)))
    (unless (atom finish)
      (setq finish-type (second finish))
      (setq finish (first finish)))
    
    (setq start (parse-program* start)
	  start-type start-type
	  finish (parse-program* finish)
	  finish-type finish-type)
    (setq ambient (getf (nthcdr 3 form) :ambient))
    (setq step-size (getf (nthcdr 3 form) :step-size))

    (when step-size
      (setq interval-class 'uniform-discrete-interval))

    (setq interval (make-instance interval-class :dimension 1
				  :start start :start-type start-type
				  :finish finish :finish-type finish-type))

    (when step-size
      (setf (step-size-of interval) (parse-program*  step-size)))

    (when ambient
      (setf (ambient-of interval) (parse-program* ambient)))
    interval))

(def-program-parser with-domain (form)
  (let ((*bindings* *bindings*))
    (loop for (var value) in (second form)
	  do (push (list var (eval value)) *bindings*))
    (parse-program* (third form))))

;;; The following function takes domain and a name and creates an element of
;;; *general* which has that name.

(defmethod make-element-named ((domain weyli::domain) name)
  (coerce name *general*))

(defmethod  make-element-named ((domain function-space) name)
  (weyli::make-function *general* name
			(weyli::dimension-of (funct-domain-of domain))))

(def-program-parser constrain (form)
  (make-constrain
    (loop for var in (second form)
	  collect (parse-program* var))
    (loop for eqn in (third form)
	  collect (parse-program* eqn))
    (parse-program* (add-sequence-if-necessary (nthcdr 3 form)))))

(def-program-parser maximize (form)
  (make-maximize
    (loop for var in (second form)
	  collect (parse-program* var))
    (loop for eqn in (third form)
	  collect (parse-program* eqn))
    (parse-program* (add-sequence-if-necessary (nthcdr 3 form)))))

(def-program-parser minimize (form)
  (make-minimize
    (loop for var in (second form)
	  collect (parse-program* var))
    (loop for eqn in (third form)
	  collect (parse-program* eqn))
    (parse-program* (add-sequence-if-necessary (nthcdr 3 form)))))

(def-program-parser forall (form)
  (let ((*bindings* *bindings*)
	(var (second form))
	var-val)
    (cond ((atom var)
	   (setq var-val (coerce var *general*))
	   (push (list var var-val) *bindings*))
	  ((eql (first var) 'wrm::pt)
	   (setq var-val
		 (make-instance 'weyli::free-module-element
			:domain *general*
			:values (loop for v in (rest var)
				      for g-var = (coerce v *general*)
				      do (push (list v g-var) *bindings*)
				      collect g-var))))
	    (t (error "Don't understand ~S in a Forall statement"
		      var)))
      (apply #'make-union var-val
	     (parse-program* (third form))
	     (loop for exp in (nthcdr 3 form)
		   collect (parse-program* exp)))))
  
(def-program-parser eqn= (form)
  (eqn= (parse-program-arith (second form))
	(parse-program-arith (third form))))
  
(def-program-parser eqn> (form)
  (eqn> (parse-program-arith (second form))
	(parse-program-arith (third form))))
  
(def-program-parser eqn>= (form)
  (eqn>= (parse-program-arith (second form))
	(parse-program-arith (third form))))

(def-program-parser + (form)
  (let ((ans (parse-program-arith (second form))))
    (loop for sub in (rest (rest form))
	  do (setq ans (+ ans (parse-program-arith sub))))
    ans))

(def-program-parser - (form)
  (let ((ans (parse-program-arith (second form))))
    (if (null (rest (rest form)))
	(setq ans (- ans))
	(loop for sub in (rest (rest form))
	      do (setq ans (- ans (parse-program-arith sub)))))
    ans))

(def-program-parser * (form)
  (let ((ans (parse-program-arith (second form))))
    (loop for sub in (rest (rest form))
	  do (setq ans (* ans (parse-program-arith sub))))
    ans))

(def-program-parser / (form)
  (let ((ans (parse-program-arith (second form))))
    (if (null (rest (rest form)))
	(setq ans (/ ans))
	(loop for sub in (rest (rest form))
	      do (setq ans (/ ans (parse-program-arith sub)))))
    ans))

(def-program-parser expt (form)
  (expt (parse-program-arith (second form))
	     (parse-program-arith (third form))))

(def-program-parser fn (form)
  (apply #'weyli::make-ge-funct *general* (second form)
	 (loop for arg in (rest (rest form))
		 collect (parse-program-arith arg))))

(def-program-parser sin (form)
  (sin (parse-program-arith (second form))))

(def-program-parser cos (form)
  (cos (parse-program-arith (second form))))

(def-program-parser abs (form)
  (abs (parse-program-arith (second form))))

(def-program-parser max-pair (form)
  (max (parse-program-arith (second form))
	(parse-program-arith (third form))))

(def-program-parser min-pair (form)
  (min (parse-program-arith (second form))
	(parse-program-arith (third form))))

(def-program-parser aref1 (form)
  (aref1 (coerce (second form) *general*)
	(parse-program-arith (third form))))

(def-program-parser aref2 (form)
  (aref2 (coerce (second form) *general*)
	(parse-program-arith (third form))
	(parse-program-arith (fourth form))))

(def-program-parser deriv (form)
  (apply #'deriv (parse-program* (second form))
         (mapcar #'parse-program-arith (nthcdr 2 form))))

(def-program-parser direct-sum (form)
  (apply #'get-direct-sum (mapcar #'parse-program* (rest form))))

;; For now there can only be one iterator
(def-program-parser loop (form)
  (let ((iterators (second form))
	(body (rest (rest form)))
        (loop-lab (new-parser-label))
        (end-lab (new-parser-label)))
    
    (when (rest iterators)
      (error "Can't deal with more than one interator yet"))
    (unless (eql (first (first iterators)) :in)
      (error "Can't deal with anything other than interval elements yet"))
    (let ((var (second (first iterators)))
	  (set (parse-program* (third (first iterators)))))     
      (parse-program*
       `(wrm::bind ((,var ,(if (eql :closed (start-type-of set))
                            (start-of set)
                            (+ (start-of set) (step-size-of set)))))
	   (wrm::sequence
	     ,loop-lab
	     (wrm::when ,(if (eql :closed (finish-type-of set))
			     `(eqn> ,var ,(finish-of set))
			     `(eqn>= ,var ,(finish-of set)))
	       (wrm::go ,end-lab))
	     ,@body
	     (wrm::set! ,var (wrm::+ ,var ,(step-size-of set)))
	     (wrm::go ,loop-lab)
	     ,end-lab))))))

(def-program-parser loop-int (form)
  (unless (eql (first (second form)) :in)
    (error "Can't parse loop-int with non interval."))
  (let ((var (coerce (second (second form)) *general*))
	(start (parse-program-arith (third (second form))))
	(end (parse-program-arith (fourth (second form))))
	(body (parse-program* (third form))))
    (make-loop-int var
		   (weyli::make-integer-sequence start end)
		   body)))

(def-program-parser if (form)
  (make-conditional (parse-program* (nth 1 form))
                    (parse-program* (nth 2 form))
                    (parse-program* (nth 3 form))))

(def-program-parser when (form) 
  (parse-program* `(wrm::if ,(second form)
                     ,(if (null (nthcdr 3 form))
                        (third form)
                        `(sequence ,@(nthcdr 2 form))))))

(def-program-parser bind (form)
  (let* ((new-bindings ())
	 (bindings
	   (loop for (var val type) in (second form)
		 for ptype = (parse-program* type)
		 for var-obj = (if (typep ptype 'function-space)
				   (weyli::make-function *general* var
					 (weyli::dimension-of (funct-domain-of ptype)))
				   (coerce var *general*))
		 do (push (list var var-obj) new-bindings)
		 collect (make-binding var-obj (parse-program* val) ptype)))
	 (*bindings* (append new-bindings *bindings*)))
    (make-bind bindings
	       (parse-program* (add-sequence-if-necessary (nthcdr 2 form))))))

(def-program-parser bind-type (form)
  (let* ((new-bindings ())
	 (bindings (loop for (var val) in (second form)
			 for var-obj = (coerce var *general*)
			 for v = (parse-program* val)
			 do (push (list var v) new-bindings)
			 collect (make-binding var-obj v)))
	 (*bindings* (append new-bindings *bindings*)))
    (make-bind bindings
	       (parse-program* (add-sequence-if-necessary (nthcdr 2 form))))))

(def-program-parser bind* (form)
  (let* ((*bindings* *bindings*))
    (labels ((make-prog (bindings)
	       (cond ((null bindings)
		      (parse-program*
			(add-sequence-if-necessary (nthcdr 2 form))))
		     (t (make-bind (list (first bindings))
				   (make-prog (rest bindings)))))))
      (make-prog (loop for (var val) in (second form)
		       for var-obj = (coerce var *general*)
		       for v = (parse-program* val)
		       do (push (list var var-obj) *bindings*)
		       collect (make-binding var-obj v))))))

(def-program-parser bind-type* (form)
  (let* ((*bindings* *bindings*))
    (labels ((make-prog (bindings)
	       (cond ((null bindings)
		      (parse-program*
			(add-sequence-if-necessary (nthcdr 2 form))))
		     (t (make-bind (list (first bindings))
				   (make-prog (rest bindings)))))))
      (make-prog (loop for (var val) in (second form)
		       for var-obj = (coerce var *general*)
		       for v = (parse-program* val)
		       do (push (list var v) *bindings*)
		       collect (make-binding var-obj v))))))

#+ignore
(def-program-parser letrec (form)
  (let ((*bindings* *bindings*))
    (loop for (var) in (second form)
	  do (push (list var (coerce var *general*)) *bindings*))
    (make-bind (loop for (var value) in (second form)
		     collect (make-binding (parse-program* var)
					   (parse-program* value)))
	       (parse-program* (add-sequence-if-necessary (nthcdr 2 form))))))

(def-program-parser sequence (form) 
  (make-sequence (mapcar #'(lambda (x)
			     (if (atom x) (make-label x)
			         (parse-program* x)))
			 (rest form)))
  #+ignore 
  (if (null (rest (rest form)))
      (parse-program* (second form))
      (make-sequence (mapcar #'(lambda (x)
			         (if (atom x) (make-label x)
				     (parse-program* x)))
			     (rest form)))))

(def-program-parser go (form)
  (make-goto (second form)))

(def-program-parser set! (form)
  (make-assignment (parse-program* (second form))
		   (parse-program* (third form))))


;; Geometry

(def-program-parser circle (form)
  (declare (ignore form))
  (weyli::circle :go nil :size-list '(circle-boundary 1.0)))

(def-program-parser pt (form)
  (make-instance 'weyli::free-module-element
	  :domain *general*
	  :values (mapcar #'(lambda (x) (coerce x *general*)) (rest form))))
