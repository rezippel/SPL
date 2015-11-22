;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;	     Code walking tools of Initial Value Problems
;;; ===========================================================================
;;; (c) copyright 1997,1993 Cornell University

;;; $id: upolynomial.lisp,v 2.11 1992/05/15 15:55:08 rz exp $

(in-package "WRMI")

;; The touch function is used to indicate which quantities need to
;; have been computed at this point in time.
(defun make-touch (exprs)
  (make-application '|Touch| exprs))

(defun touch? (form)
  (and (typep form 'application)
       (eql (funct-of form) '|Touch|)))

;;; Now we want to insert the touches that are necessary.  The basic
;;; idea is to first walk the program untill we reach a SEQUENCE.
;;; Each form in the sequence is then examined for dependence on
;;; constrained variables, and if so, a TOUCH is ibnserted prior to the
;;; tagbody.  Expressions are examined for dependence by walking till
;;; we find a Weyl expression that is :eval'ed.

(defvar *allowed-constraints* nil
  "Indicates which types of constraints are viewed as constraining the varables")

(defvar *constrained-vars* nil)
(defvar *touched-vars* nil
  "The variables that have already been touched at this point in the program")

(defvar *lift-functions* nil
  "Indictes whether constrained functions should be lifted to up through contours. This is a KLUDGE")

(defun mark-constrained-vars (prog &optional
			       (*allowed-constraints* '(:constrain))
			       (*lift-functions* nil))
  (let ((*touched-vars* nil)
	(*constrained-vars* nil))
    (prog1
      (walk-form prog #'mark-constrained-vars-walker)
      (unless (null *constrained-vars*)
	(error "Uncaptured variables ~S" *constrained-vars*)))))

(defun mark-constrained-vars-walker (form context env)
  (let (terms)
       (labels ((strip-args (var env)
		(if (and (ge-application? var)
			 (not (loop for arg in (args-of var)
				    when (env-lookup arg :bind env)
				      return t)))
		    (funct-of var)
		    var))
	      (walk-sub-term (term env) 
	       (let ((up-vars nil)
		     (these-vars nil))
		    (prog1 
		      (let ((*constrained-vars* nil))
			   (setq term (walk-form-internal term context env))
			   (loop for var in *constrained-vars* do
			     (cond ((and *lift-functions*
					 (ge-application? var)
					 (loop for arg in (args-of var)
					       when (env-lookup arg :bind env)
						 return t))
				    (push var up-vars))
				   (t (push var these-vars))))
			   (setq these-vars
				 (loop for var in these-vars
				       collect (strip-args var env)))
			   (unless (null these-vars)
			     (push (make-touch these-vars) terms))
			   (push term terms)
			   )
		      (setq *constrained-vars*
			    (append up-vars *constrained-vars*))))))
	     (cond ((typep form 'sequence)
		    (loop for term in (terms-of form) do 
		      (cond ((typep form 'label)
			     (push term terms))
			    (t (walk-sub-term term env))))
		    (cond ((equal terms (terms-of form))
			   form)
			  (t (values (make-sequence1 (reverse terms))
				     :done))))
		   ((touch? form)
		    (loop for expr in (args-of form)
			  do (push expr *touched-vars*))
		    form)
		   ((member form *touched-vars* :test #'ge-equal)
		    form)
		   ((ge-variable? form)
		    (when (env-constraints env form)
		      (push form *constrained-vars*))
		    form)
		   ((ge-application? form)
		    (when (or (env-constraints env (funct-of form))
			      (env-constraints env form))
		      (push form *constrained-vars*))
		    form)
		   ((typep form 'label)
		    (values form t))
		   ((typep form 'bind)
		    (values (walk-bind-internal form env
						#'(lambda (form env)
							  (walk-sub-term form env)
							  (make-sequence1 (reverse terms))))
			    :done))
		   ((typep form 'constrain)
		    (cond ((member :constrain *allowed-constraints*)
			   (let ((vars (vars-of form))
				 (constraints (constraints-of form)))
				(push (list vars :constraint constraints) env)
				(walk-sub-term (term-of form) env)
				(values
				  (make-constrain vars constraints
						  (make-sequence1 (reverse terms)))
				  :done)))
			  (t (walk-sub-term (term-of form) env)
			     form)))
		   ((typep form 'extremize)
		    (cond ((member (constraint-type form) *allowed-constraints*)
			   (let ((vars (vars-of form))
				 (expressions (expressions-of form)))
				(push (list vars (constraint-type form) expressions) env)
				(walk-sub-term (term-of form) env)
				(values
				  (funcall (if (typep form 'minimize) #'make-minimize
					       #'make-maximize)
					   vars expressions 
					   (make-sequence1 (reverse terms)))
				  :done)))
			  (t (walk-sub-term (term-of form) env)
			     form)))
		   (t form)))))

(defun make-sequence1 (terms)
  (let ((new-terms nil) touches)
    (loop for term in terms do
      (unless (null term)
	(cond ((touch? term)
	       (loop for expr in (args-of term)
		     do (pushnew expr touches :test #'ge-equal)))
	      (t (when touches
		   (push (make-touch touches) new-terms)
		   (setq touches nil))
		 (push term new-terms)))))
    (when touches
      (push (make-touch touches) new-terms))
    (if (null (rest new-terms))
	(first new-terms)
	(make-sequence (reverse new-terms)))))

(defun collect-touched-exprs (sequence)
  (let ((vars nil))
    (loop for form in (terms-of sequence)
          do (when (touch? form)
	       (loop for arg in (args-of form)
		     do (pushnew arg vars :test #'ge-equal))))
    vars))



;;; The following is assumed to be given a touch and returns the list
;;; of forms which the touch is to be replaced by when doing an
;;; initial value advancement.

(defun iv-advance-touch (form env) 
  (let (dfuncs    ;; The discrete functions corresponding to these expressions
	mvar
	new-funcs
	constraints
        micro-steps)
    (unless (touch? form)
      (error "~S was expected to be a touch" form))
    ;; Can only handle the situation where each touch consists of
    ;; functions of the same argument.
    (loop for fun in (args-of form)
          for disc-fun = (env-lookup (and (ge-application? fun) (funct-of fun))
                                     :discrete-function env)
	  with dfun and new-arglist
	  do (unless disc-fun
	       (error "Expected ~S to be a discretized application" fun))

             (setq dfun (first disc-fun))
             (setq new-arglist (copy-list (args-of fun)))
             (setq mvar (getf (cl:nth (second disc-fun) new-arglist)
                              :micro-step))
             (pushnew mvar micro-steps)
                          
             (setq new-arglist (copy-list (args-of fun)))
             (setf (cl:nth (second disc-fun) new-arglist)
                   (+ 1 (disc-var-of mvar)))
	     (push (apply dfun new-arglist) new-funcs)

             (push dfun dfuncs))

    (setq dfuncs (reverse dfuncs))
    (unless (null (rest micro-steps))
      (error "Can only deal with one discretized dimension at a time: ~S"
             micro-steps))

    (loop for constraint in (apply #'env-constraints env dfuncs)
	  do (loop for c in (extract-constraints constraint dfuncs mvar)
		   do (unless (typep c 'weyli::local-error-approximation)
		        (push c constraints))))
    (advance-step *iv-stepper* mvar new-funcs constraints)
    #+ignore
    `(,label
      ,(make-conditional (eqn> (getf  mvar :macro-step) 
                               (cont-var-of mvar))
         (make-sequence
	   ;; Within this sequence we are only interested in
	   ;; constraints that hold for instants within a given time
	   ;; interval.
	   `(
	     ;; FIXTHIS The following statement is a quick and dirty kludge
	     ,(make-assignment (first (steps-of mvar)) 0.01)
	     ;; ,(make-application 'choose-u-step (list 'time mvar))
	     ,(make-constrain new-funcs constraints (make-touch new-funcs))
	     ,@(advance-forms mvar)
	     ,(make-goto (name-of label))))))))

(defun relevant-functions (expr)
  (let (functs)
    (walk-form expr
      #'(lambda (form context env)
          (declare (ignore context))
          (when (ge-application? form)
            (let ((funct (funct-of form))
                  (args (args-of form)))
              (when (ge-function-deriv? funct)
                (setq funct (weyli::make-function (domain-of form)
                                                  (name-of funct)
                                                  (nargs-of funct))))
              (pushnew (list (apply funct args)
                             (loop for arg in args
                                   collect (env-lookup arg :element env)))
                       functs		     
                       :test #'(lambda (x y) (ge-equal (first x) (first y))))))
          form))
    functs)) 

;; EXTRACT-CONSTRAINTS takes a single weyl-expression (which includes
;; quantified sets) and extracts the expression(s) that quantify the
;; variables.

;; If we don't know anything else, return the empty list.
(defmethod extract-constraints ((expr t) vars disc-var)
  (declare (ignore vars disc-var))
  ())

(defmethod extract-constraints ((eqs universal-quantified-set) funcs disc-var)
  (let (constraints
        new-bindings)
    (loop for (scoped-var set) in (bound-vars-of eqs)
          do 
          (if (typep scoped-var 'iv-cluster)
            (loop for eq in (exprs-of eqs) do
                  (loop for fun in funcs
                        do (when (funct-depends? fun eq)
                             (push (substitute (disc-var-of disc-var)
                                               (disc-var-of scoped-var)
                                               eq)
                                   constraints)
		              (return t))))
            (push (list scoped-var set) new-bindings)))
    (if (null new-bindings)
      constraints
      (list 
       (make-universal-quantified-set (domain-of eqs)
                                      new-bindings constraints)))))
    
(defmethod funct-depends? ((func weyli::ge-function) constraint)
  (let ((depends? nil))
    (walk-form constraint
      #'(lambda (f c e)
	  (declare (ignore c e))
	  (when (and (ge-application? f)
		     (eql (funct-of f) func))
	    (setq depends? t))
	f))
    depends?))

(defmethod funct-depends? ((var weyli::ge-application) constraint)
  (funct-depends? (funct-of var) constraint))

;; Cleanup program: Remove all constraints that aren't necessary.
;; Remove all touches.

(defvar *noticed-constrained-exprs*)
(defvar *assigned-constrained-exprs*)
(defvar *type-dependencies*)

;; This is a different use of the following global.
;;(defvar *constrained-vars*)

(defun function-eq? (var form)
  (or (ge-equal var form)
      (if (and (ge-function? var)
               (ge-application? form))
        (ge-equal var (funct-of form)))))

(defun cleanup-program-walker (form context env)
  (cond ((eql context :math-funct) 
		(pushnew form *noticed-constrained-exprs*)
		(values form :done))
	((typep form 'weyli::general-expression) 
	 (cond ((and (member form *constrained-vars* :test #'ge-equal)
		       (not (member form *assigned-constrained-exprs*
				    :test #'ge-equal)))
		(pushnew form *noticed-constrained-exprs*)
		(values form :done))
	       (t form)))
        ((typep form 'assignment)
         (setq *noticed-constrained-exprs* 
               (delete (location-of form) *noticed-constrained-exprs*
                       :test #'ge-equal))
         (push (location-of form) *assigned-constrained-exprs*)
         (walk-form-internal (value-of form) context env)
         form)
	((or (typep form 'constrain)
             (typep form 'extremize))
	 (let* ((vars (vars-of form))
		(*constrained-vars* (append vars *constrained-vars*))
		(term 
		 (walk-form-internal (term-of form) context env)))
	   (cond ((loop for v in vars
			do (when (member v *noticed-constrained-exprs*)
			     (return t)))
		  (values
                   (cond ((typep form 'constrain)
                          (make-constrain vars (constraints-of form) term))
                         ((typep form 'maximize)
                          (make-maximize vars (expressions-of form) term))
                         ((typep form 'minimize)
                          (make-minimize vars (expressions-of form) term))
                         (t (error "What is this ~S?" form)))
                   :done))
		 (t (values term :done)))))
	((typep form 'bind)
	 (let ((bindings (bindings-of form))
               new-bindings
	       term)
	   (loop for b in bindings
		 do (push (var-of b) *constrained-vars*))
	   (setq term (walk-form-internal (term-of form) context env))
	   (cond ((progn 
                    (loop for b in bindings
			  do (when (or (member (var-of b) 
                                               *noticed-constrained-exprs*
                                               :test #'function-eq?)
                                       (member (var-of b) 
                                               *assigned-constrained-exprs*
                                               :test #'function-eq?)
				       (member (var-of b)
					       *type-dependencies*
					       :test #'(lambda (x y) 
							 (eql x (first y)))))
                               (push b new-bindings)))
                    new-bindings)
		  (loop for b in bindings
			do (setq *noticed-constrained-exprs*
				 (delete (var-of b) *noticed-constrained-exprs*)))
		  (values (make-bind (nreverse new-bindings) term)
			  :done))
		 (t (values term :done)))))	     
	((and (eql context :eval) (touch? form))
	 (let ((noticed *noticed-constrained-exprs*))
	   (mapcar #'(lambda (term) (cleanup-program-walker term context env))
		   (args-of form))
	   (if (eql noticed *noticed-constrained-exprs*)
             nil
	     (values form :done))))
	(t form)))

(defun cleanup-program (prog)
  (let ((*noticed-constrained-exprs* nil)
        (*assigned-constrained-exprs* nil)
	(*constrained-vars* nil)
	(*type-dependencies* nil))
    (setq *type-dependencies* (cleanup-type-dependencies prog))
    (walk-form prog #'cleanup-program-walker)))

;; The cleanup-type-dependency routine determines the type dependecies
;; between variables and types in a program.  This information is
;; managed in *type-dependencies* which is a list of triples, the name
;; of the type, a list of depdendent types and a list of variables of
;; that type.  (This is only done for the domin types, since that's
;; all we have now, and all we will have after the cataclysm.)

(defun cleanup-type-dependencies-walker (form context env)
  (cond ((typep form 'bind)
	 (loop for b in (bindings-of form)
	       do (cond ((null (type-of b))
			 ;; This is a type binding
			 (when (typep (value-of b) 'weyli::domain)
			   (push (list (var-of b) (value-of b) nil nil)
				 *type-dependencies*)
			   (when (or (typep (value-of b) 'has-ambient)
				     (typep (value-of b) 'function-space))
			     (loop for td in *type-dependencies*
				   with subtype = 
				     (if (typep (value-of b) 'has-ambient) 
					 (ambient-of (value-of b))
					 (funct-domain-of (value-of b)))
				   do (when (eql subtype (second td))
					(push (var-of b) (cl:third td)))))))
			(t (add-var-type-dep b))))
	 form)
	(t form)))

(defun add-var-type-dep (bind)
  (let ((type (domain-of (type-of bind)))
	 dom-type) 
    (if (not (typep type 'weyli::domain))
	(setq type (domain-of type)))
    (setq dom-type (and (typep type 'weyli::function-space)
			(funct-domain-of type)))
    (loop for td in *type-dependencies*
	  do (when (or (eql type (second td))
		       (eql dom-type (second td)))
	       (push (var-of bind) (cl:fourth td))))))

(defun cleanup-type-dependencies (prog)
  ;; Returns *type-dependencies* 
  (let ((*type-dependencies* nil)
	(repeat? t))
    (walk-form prog #'cleanup-type-dependencies-walker)
    ;; Remove the types that are no longer necessary
    (loop while repeat? 
	  do (loop for td in *type-dependencies*
		   do (when (and (null (third td)) (null (fourth td)))
			(setq *type-dependencies* 
			      (delete td *type-dependencies*))
			(loop for td2 in *type-dependencies*
			      do (setf (cl:third td2) 
				       (delete (first td) (cl:third td2))))
			(setq repeat? t)
			(return t))
		   finally (setq repeat? nil)))
    *type-dependencies*))
