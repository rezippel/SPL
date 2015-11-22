;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;	     Code walking tools of Initial Value Problems
;;; ===========================================================================
;;; (c) copyright 1997,1993 Cornell University

;;; $id: upolynomial.lisp,v 2.11 1992/05/15 15:55:08 rz exp $

(in-package "WRMI")

;; All initial value transforms are instances of the following class.  In
;; addition to the standard information, we also maintain the order of the 
;; local error introduced. 

(defclass iv-transform (spl-transform)
     ((local-error-order :initarg :local-error-order
			 :accessor local-error-order-of)))

(defmethod print-object ((obj iv-transform) stream)
  (format stream "#<IV Transform: ~A (~D)>"
	  (name-of obj)
	  (local-error-order-of obj)))

;; As initial value transforms are defined, they are added to the following
;; list.  This list is used to generate the IV transform dialog.
(defvar *IV-transforms* ())

(defmethod menu-name-of ((obj iv-transform))
  (format nil "~A (~D)" (name-of obj) (local-error-order-of obj)))

(defmacro define-iv-transform (name args params &rest body)
  (let* ((iv-name (or (getf params :name) `',name))
	 (order (getf params :local-error-order))
	 (transform (getf params :transform))
	 (transform-name (or transform
			     (intern (format nil "~A-IV-transform" name))))
	 (iv-trans-name (intern (format nil "IV-~A" name))))
    (unless (or transform body)
      (error "Expected a transform function to be provided in ~A"
	     name))
    `(progn
       (eval-when (compile load eval)
	 (proclaim '(special ,iv-trans-name)))
       ,@(unless transform
	   `((defun ,transform-name ,args ,@body)))
       (setq ,iv-trans-name
	   (make-instance 'iv-transform
			  :name ,iv-name
			  :transform ',transform-name
			  :local-error-order ,order))
       (pushnew ,iv-trans-name *iv-transforms*
                :test #'(lambda (x y) (eql (transform-of x) (transform-of y))))
       ',iv-trans-name)))

;; The following transform captures the time-step decisions and time
;; advancing.
(defclass iv-stepper (spl-transform)
     ())

(defmethod print-object ((obj iv-stepper) stream)
  (format stream "#<IV Stepper: ~A>"
	  (name-of obj)))

(defmethod pretty-name-of ((obj iv-stepper))
  (string (name-of obj)))

(defmethod validate-stepper ((stepper iv-stepper))
  nil)

;; As initial value steppers are defined, they are added to the
;; following list.  This list is used to make up the IV Transform dialog.

;; Each IV-stepper is its own class.  In fact there is just one instance of
;; each class created...
(defvar *IV-steppers* ())

;; If :stepper-class is provided, then no method or class is created, just 
;; a new instance of the specified class.

(defmacro define-iv-stepper (name args params &rest body)
  (let* ((iv-name (or (getf params :name) name))
	 (stepper-class (getf params :stepper-class))
	 (iv-trans-name (intern (format nil "IV-~A" name))))
    (unless (or stepper-class body)
      (error "Expected a transform function to be provided in ~A"
	     name))
    `(progn
       (eval-when (compile load eval)
	 (proclaim '(special ,iv-trans-name)))
       ,@(unless stepper-class
           (setq stepper-class iv-name)
           `((defclass ,stepper-class (iv-stepper) 
               ,(getf params :instance-vars))
	     (defmethod advance-step ((stepper ,stepper-class) ,@args)
               ,@body)))
       (setq ,iv-trans-name
	   (make-instance ',stepper-class
			  :name ',iv-name
                          ,@(getf params :init-args)))
       ,@(unless (getf params :method-only)
          `((pushnew ,iv-trans-name *iv-steppers*
                     :test #'(lambda (x y) (eql (name-of x) (name-of y))))))
       ',iv-trans-name)))


;; The following macro takes care of the domain definitions for
;; required for dealling with time.  The domain TIME is all time,
;; while TIME-DOMAIN is the causal portion of time.
(def-program-parser with-causal-time (form)
  (parse-program*
   `(wrm::bind-type* ((wrm::time (wrm::euclidean-space 1))
		      (wrm::time-domain
		       (wrm::interval (0.0 :closed)
				      (wrm::*positive-infinity* :open)
				      :ambient wrm::time)))
      ,@(rest form))))


;; The relationship between the continuous and discrete variables is
;; represented by an IV-Cluster object.  The structure consists of the
;; continuous variable, the corresponding discrete variable and list
;; of the step sizes (for multistep methods).

;; IV-clusters are Weyl general expression atoms so that they can be used
;; in the same place as variables in Weyl expressions.  [Why they have a
;; property list is beyond me at the moment  --RZ]
(defclass IV-cluster (weyli::general-expression weyli::ge-atom
						weyli::has-property-list)
     ((cont-var :initarg :cont-var :accessor cont-var-of)
      (disc-var :initarg :disc-var :accessor disc-var-of)
      (steps :initarg :steps :accessor steps-of)))

(defmethod initialize-instance :after ((obj Iv-Cluster) &rest plist)
  (declare (ignore plist))
  (setf (cont-var-of obj) (coerce (cont-var-of obj) *general*))
  (setf (disc-var-of obj) (coerce (disc-var-of obj) *general*))
  (setf (steps-of obj) (loop for step in (steps-of obj)
			     collect (coerce step *general*))))

(defmethod print-object ((obj iv-cluster) stream)
  (format stream "#<IV(~D): ~S + ~S, ~S>"
	  (length (steps-of obj))
	  (cont-var-of obj) (first (steps-of obj))
	  (disc-var-of obj)))

(defun make-iv-cluster (domain cont-var steps &optional disc-var)
  (unless (ge-variable? cont-var)
    (unless (symbolp cont-var)
      (error "Don't know how to deal with CVar: ~S" cont-var))
    (setq cont-var (coerce cont-var domain)))
  (unless (listp steps)
    (when (symbolp steps)
      (setq steps (coerce steps domain)))
    (setq steps (list steps)))
  (when (null disc-var)
    (setq disc-var (coerce (intern
			     (format nil "~A-N" (weyli::symbol-of cont-var)))
			   domain)))
  (when (symbolp disc-var)
    (setq disc-var (coerce disc-var *general*)))
  (make-instance 'iv-cluster :cont-var cont-var
		 :disc-var disc-var :steps steps))

;; This routine returns the forms for advancing the current time step by 
;; one unit.  The step sizes are also shifted.  The most recent step size 
;; is left unchanged.
(defmethod advance-forms ((var iv-cluster))
  (let ((cont-var (cont-var-of var))
	(disc-var (disc-var-of var))
	(steps (steps-of var)))
    (list* (make-assignment cont-var (+ cont-var (first steps)))
	   (make-assignment disc-var (+ 1 disc-var))
	   (reverse
	     (loop for (h last-h) on steps
		   when (not (null last-h))
		     collect (make-assignment last-h h))))))

;; Each time a function is discretized by DISCRETIZE-FUNCTION, the
;; function is pushed on the list *DISCRETIZED-FUNCTIONS*.  This
;; variable is typically used to determine which functions have been
;; discretized in the process of discretizing a constraint.

;; Each entry of this list is a triple consisting of the function that
;; was discretized, the argument list, and the argument index that was
;; discretized.

;; This list is maintained in a scoped fashion and is not bound globally.
(defvar *discretized-functions*)

(defun add-new-discretized-function (fun args index)
  (when (typep fun 'weyli::ge-function-deriv)
    (setq fun (weyli::make-function (domain-of fun) (name-of fun)
				    (nargs-of fun))))
  (loop for (f a i) in *discretized-functions*
	do (when (and (eql fun f)
		      (weyli::ge-lequal a args)
		      (eql index i))
	     (return t))
	finally (push (list fun args index) *discretized-functions*)))

;; Counter to ensure we always generate new labels during code 
;; transformations
(defvar *label-counter*)

;; DISCRETIZE-FUNCTION takes a Weyl function and an argument index,
;; and converts it to a discretized function, establishing all the
;; appropriate linkages.

;; If a function is a discretization of another function, then the
;; :discretized properties indicates in which argument positions it
;; has been discretized.

;; The :continuous-fun property is the fully continuous function from
;; which the discrete function is derived.

;; The :discretizations property appears only on continuous functions.
;; It is a list of those discrete functions have been defined corresponding to
;; discretizations of different arguments.
(defmethod discretize-function ((fun weyli::ge-function) &rest args)
  (let* ((discretizations (getf fun :discretized))
	 (cfun (if (null discretizations) fun
		   (getf fun :continuous-fun)))
	 (nargs (nargs-of cfun)))
    (loop for arg in args
	  do (unless (> nargs arg -1)
	       (error "Illegal argument index for ~S, ~D"
		      fun arg)))
    (when (intersection args discretizations)
      (error "~S is already discrete in positions ~S" fun
	     (intersection args discretizations)))
    (get-discretized-function cfun (append args discretizations))))

(defmethod discretize-function ((fun weyli::ge-function-deriv) &rest args)
  (let ((cont-derivs)			;Continuous derivs
	(disc-derivs))
    (loop for d in (derivs-of fun)
	  do (if (member d args)
		 (push d disc-derivs)
		 (push d cont-derivs)))
    (values
      (weyli::make-function-deriv 
	(apply #'discretize-function
	       (weyli::make-function (domain-of fun) (name-of fun)
				     (nargs-of fun))
	       args)
	cont-derivs)
      disc-derivs)))

(defun get-discretized-function (cfun discretizations)
  (let ((nargs (nargs-of cfun))
	dfun lookup)    
    (setq discretizations (sort discretizations #'cl:<))
    (cond ((null discretizations)
	   (setq dfun cfun))
	  ((setq lookup (assoc discretizations (getf cfun :discretizations)
                               :test #'equal))
	   (setf dfun (second lookup)))
	  (t (setq dfun (weyli::make-function (domain-of cfun)
		          (format nil "~A-[~{~S~}]"
				  (weyli::name-of cfun) discretizations)
			  nargs))
	     (setf (getf dfun :discretized) discretizations)
	     (push (list discretizations dfun) (getf cfun :discretizations))
	     (setf (getf dfun :continuous-fun) cfun)))
    dfun))

;; CONTINUOUS-FUNCTION returns the corresponding function that is
;; continuous in the specified argument positions.  
(defun continuous-function (fun &rest args)
  (let* ((discretizations (getf fun :discretized))
	 (cfun (if (null discretizations) fun
		   (getf fun :continuous-fun))))
    (loop for arg in args
	  do (unless (member arg discretizations)
	       (error "~S is not discretized in arg ~D" fun arg)))
    (get-discretized-function cfun (loop for d in discretizations
					 unless (member d args)
					   collect d))))

;; These routines are used by the discretization formulas.  They take a single
;; expression and discretize it in all of the domains that have been 
;; discretized.  N indicates the offset to be used in the discretization 
;; process.  

(defun iv-discretize (exp env n) 
  (labels
    ((discretize (form context env) 
       (declare (ignore context)) 

       (cond ((ge-variable? form)
	      (let* ((iv (first (env-lookup form :discrete-variable env)))
		     (var (cont-var-of iv)))
		(values 
		  (cond ((= n 1) (+ var (first (steps-of iv))))
			((= n 0) var)
			((> n 1) 
			 (error "Discretization offset is too large: ~D" n))
			(t (loop for i below (- n)
				 for step in (rest (steps-of iv))
				 with v = var
				 do (setq v (- v step))
				 finally (return v))))
		  :done)))
	     ((ge-application? form)
	      (iv-discretize-application form env n))
	     (t form))))
    (walk-form exp #'discretize :env env)))

(defun iv-discretize-application (form env n)
  (flet ((discretize-arg (exp)
	   (cond ((ge-variable? exp)
		  (let ((iv (env-lookup exp :discrete-variable env)))
		    (if (null iv)
			(error "In ~S, expected variable ~S to be discretized"
			       form exp)
			(first iv))))
		 ((number? exp) exp)
		 (t (error "Expected a single variable here: ~S" exp)))))
    (multiple-value-bind (dfun disc-argn derivs)
	(iv-discretize-function (funct-of form) env) 
      (cond ((null dfun)
	     (values form :recurse))
	    ((cl:zerop derivs)
	     (let* ((arglist (copy-list (args-of form)))
		    (iv (discretize-arg (nth disc-argn arglist))))
	       (cond ((number? iv)
		      (setf (cl::nth disc-argn arglist) iv))
		     ((typep iv 'iv-cluster)
		      (setf (cl::nth disc-argn arglist)
			    (+ (disc-var-of iv) n))
		      ;; Tell other people that we have discretized this
		      ;; function with this set of arguments.
		      (add-new-discretized-function (funct-of form)
						    (args-of form)
						    disc-argn))
		     (t (error "Expected IV-CLUSTER here: ~S" form)))
	       (values (apply dfun arglist)
		       :done)))
	    (t
	     (let* ((arglist (args-of form))
		    (new-arglist (copy-list arglist))
		    (iv (discretize-arg (nth disc-argn arglist)))
		    disc-arg sum)
	       (unless (typep iv 'iv-cluster)
		 (error "Expected IV-CLUSTER here: ~S" form))

	       (setq disc-arg (+ (disc-var-of iv) n))
	       (setf (cl::nth disc-argn new-arglist) (+ 1 disc-arg))
	       ;; Tell other people that we have discretized this function
	       ;; with this set of arguments.
	       (add-new-discretized-function (funct-of form) (args-of form)
					     disc-argn)
	       	       
	       (setq sum (apply dfun new-arglist))
	       (loop for i below derivs
		     do (setf (cl::nth disc-argn new-arglist) (- disc-arg i))
			(setq sum (if (oddp i)
				      (+ sum (apply dfun new-arglist))
				      (- sum (apply dfun new-arglist)))))
	       (values (/ sum (expt (first (steps-of iv)) derivs))
		       :done)))))))

;; The following routine returns three values:
;;  (1) The discretized version of the function, 
;;  (2) the argument position that has been discretized, if any,
;;  (3) the number of times the discretized argument position has been
;;      differentiated.
(defun iv-discretize-function (funct env)
  (let ((dfun))
    (cond ((ge-function-deriv? funct)
	   (setq dfun (env-lookup (get-function (domain-of funct)
						(name-of funct))
				  :discrete-function env))
	   (values
	     (weyli::make-function-deriv
	       (first dfun) (remove (second dfun) (derivs-of funct)))
	     (second dfun)
	     (count (second dfun) (derivs-of funct))))
	  (t (setq dfun (env-lookup funct :discrete-function env))
	     (when dfun
	       (values (first dfun) (second dfun) 0))))))   

;; IV-ADVANCE finds the CONSTRAIN forms in PROG that contain
;; differential equations and applies the specified discretization
;; method to it.
(defvar *iv-transform*)

;; This is the transform to use to establish the step size.
(defvar *iv-stepper*)

;; Initial value problems this should almost always be time. 
(defvar *ambient* ()
  "The ambient space that is being discretized.")

(defvar *code-bindings* ()
  "Bindings of new variables introduced while transforming a body")

(defun ambient-spaces-of (prog)
  (let ((ambients nil))
    (walk-form prog
      #'(lambda (form context env)
          (declare (ignore context env))
          (when (and (typep form 'binding)
                     (typep (value-of form) 'weyli::dimensional-space)
                     (not (typep (value-of form) 'has-ambient))
                     (not (typep (value-of form) 'weyli::function-space)))
            (push form ambients))
          form))
    ambients))

;; Is this domain a subset of the ambient space?
(defmacro in-ambient? (space)
  `(and (typep ,space 'has-ambient)
	(eql (ambient-of ,space) *ambient*)))

(defvar *iv-steps* ()
  "List of variables used to indicate the step between time steps")

;; When variables are found that need to be discretized, they are
;; pushed onto the environment

(defun iv-advance (prog *ambient* *iv-transform* *iv-stepper*)
  (let ((*iv-steps* (list 'h))
        (*label-counter* 0)
	;; This isn't quite right.  I am assuming that the ambient
	;; space name appears in only one spot in the program and that
	;; it is only used properly lexically scoped.
	*code-bindings*)
    
    (when (symbolp *ambient*)
      (loop for bind in (ambient-spaces-of prog)
	  do (when (string= (symbol-name (weyli::symbol-of (var-of bind)))
			    (symbol-name *ambient*))
	       (setq *ambient* (value-of bind))
	       (return t))
	  finally (error "Need to specify an ambient space")))

    ;; Mark the places where constrained variables are used.  Once
    ;; this is done the entire transform can be performed by a simple
    ;; code walk. (more or less)
    (setq prog (mark-constrained-vars prog))

    (walk-form prog #'iv-advance-form)))

(defun iv-advance-form  (form context env)
  (declare (ignore context)) 

  (cond ((typep form 'constrain)
	 (iv-advance-constrain form env))
	((typep form 'bind)
	 (iv-advance-bind form env))
	((typep form 'binding) 
	 (iv-advance-binding form env))
	((typep form 'sequence)
	 (iv-advance-sequence form env))
	((ge-application? form)
	 (or (first (env-lookup form :discrete-expression env))
	     form))
	((touch? form)
	 (iv-advance-touch form env))
	(t form)))

;; This form find all bindings of functions that map from a
;; discretized domain, and introduce new functions that are discrete
;; in those arguments.  FIXTHIS: This also needs to discretize the
;; value of the function.
(defun iv-advance-binding (form env)
  (declare (ignore env))

  (let ((type (type-of form)))
    (flet ((discrete-form (form dform index) 
	     ;; Tell the rest of the world what the discrete form of
	     ;; this function is.  This indicates that the function
	     ;; should never appear in its continuous form once this
	     ;; code walk is complete.
	     (push (list (var-of form) :discrete-function dform index)
		   *new-envs*)  
	     (list form (make-binding dform nil (type-of form)))))
      (cond ((and (bi-type? type)	; If this is a function
		  (typep (domain-of type) 'function-space))  
	     ;; Then it is a function from DOMAIN to RANGE
	     (let ((domain (funct-domain-of (domain-of type)))
		   indices)
	       (cond ((typep domain 'direct-sum-dimensional-space)
		      ;; If it is a function of more than one argument,
		      ;; determine which arguments are from the ambient space.
		      (loop for i below (dimension-of domain)
			    for pspace = (ref domain i)
			    do (when (in-ambient? pspace)
				 (push i indices))) 
		      (cond ((null indices) form)
			    ((rest indices)
			     (error "Too much dependence: ~S~S"
				    (var-of form) indices))
			    (t (discrete-form
				 form
				 (discretize-function (var-of form)
						      (first indices))
				 (first indices)))))

		     ((in-ambient? domain)
                      ;; In this case the function only has one argument, so
                      ;; the use of 0 here is OK.
		      (discrete-form form
				     (discretize-function (var-of form) 0)
				     0))
		     (t form))))
            (t form)))))

(defun iv-advance-bind (form env)
  (let (*new-initial-terms* 
        *new-final-terms*
        bindings
        new-bindings term)
    (setq bindings (walk-bindings (bindings-of form) env))

    (setq term (walk-form-internal 
                (sequence-include-bind-terms (term-of form))
                :eval env))
    (loop for binding in bindings
          for match = (assoc (var-of binding) *code-bindings*)
          do (when match
               (setq *code-bindings* (delete match *code-bindings*))
               (setq new-bindings
                     (append
                      new-bindings
                      (loop for (v val type) in (rest match)
                            collect 
                            (make-binding v 
                                          (if (eql val :var) (value-of binding)
                                              val)
                                          (if (eql val :var) (type-of binding)
                                              type)))))))
    (values (make-bind (append bindings new-bindings) term)
            :done)))

(defun iv-advance-constrain (form env)
  (let ((new-vars (vars-of form))
	dfun)
    (loop for fun in (vars-of form)
	  do (when (and (ge-function? fun)
			(setq dfun (env-lookup fun :discrete-function env)))
	       (push (first dfun) new-vars)))
    (values 
      (make-constrain new-vars
		      (loop for c in (constraints-of form)
			    collect (apply-discretization c env))
		      (term-of form))
      :recurse)))

(defun apply-discretization (c env)
  (cond ((typep c 'universal-quantified-set)
	 (let ((bound-vars ())
	       (discretize? nil))
	   (setq bound-vars
		 (loop for (var set) in (bound-vars-of c)
		       with iv-var
		       collect
		       (list
			 (cond ((and (in-ambient? set)
				     ;; Make sure this quantification isn't
				     ;; already discretized
				     (not (typep var 'iv-cluster)))
				(setq discretize? t)
				(setq iv-var (make-iv-cluster *general* var
							      *iv-steps*))
				(push (list var :discrete-variable iv-var)
				      env)
				iv-var)
			       (t var))
			 set)))
	   (cond ((null discretize?)
		  (make-universal-quantified-set
		    (domain-of c) (bound-vars-of c)
		    (apply-discretization-rec (exprs-of c) env)))
		 (t (make-universal-quantified-set
		      (domain-of c) bound-vars
                      ;; Keep track of all of the functional expressions that
                      ;; have been encountered and discretized.  Then adjoin
                      ;; the appropriate local error information
                      (let ((*discretized-functions* ()))
                        (append
                         (apply-discretization-rec (exprs-of c) env)
                         (loop for (fun args index) in *discretized-functions*
                               collect (create-local-error fun args index env)))))))))
	(t (funcall (transform-of *iv-transform*) c env))))

(defun create-local-error (fun args index env)
  (let ((new-args (copy-list args))
	(arg (nth index args))
	iv)
    (cond ((and (ge-variable? arg)
		(setq iv (first (env-lookup arg :discrete-variable env))))
	   (setf (cl::nth index new-args) (disc-var-of iv))
	   (weyli::make-local-error-approximation
	     (apply fun args)
	     (apply (discretize-function fun index) new-args)
	     (first (steps-of iv))
	     (local-error-order-of *iv-transform*)))
	  (t (error "Not yet implemented ~S~S" fun args)))))

(defun apply-discretization-rec (exprs env) 
  (loop for exp in exprs
	for disc-exp = (apply-discretization exp env)
	append (if (typep exp 'set)
		   (if (listp disc-exp) disc-exp
		       (list disc-exp))
		   (if (listp disc-exp) (cons exp disc-exp)
		       (list exp disc-exp)))))

(defun iv-advance-sequence (form env)
  (let (macro-steps micro-steps new-args)
    
    ;; Figure out which independent variables are involved.
    (loop for exp in (collect-touched-exprs form)
          with micro-step and macro-step and discretization do 
          (when (and ;; It must be an application for this technique to work
                     (ge-application? exp) 
                     ;; and the function must have been discretized
                     (setq discretization
                           (env-lookup (funct-of exp) :discrete-function env))
                     ;; But only if we don't already know about the expression
                     (not (env-lookup exp :discrete-expression env)))
            (setq macro-step (nth (second discretization) (args-of exp))) 
            
            (when (not (member macro-step macro-steps :test #'ge-equal))
              
              ;; macro-step is the continuous variable that this function
              ;; depends on, so it had better be a variable.  
              (unless (weyli::ge-atom? macro-step)
                (error "Not yet implemented: ~S, Macro step = ~S" 
		       exp macro-step))
              
              ;; In terms of the steps that are taken, it will be the
              ;; MACRO-step.  Elsewhere, it is assumed that there is code
              ;; to advance MACRO-STEP.  MICRO-STEP is the discrete variable
              ;; that we are introducing and corresponds to the
              ;; MICRO-step.  If we didn't have adaptive stepsizing, this
              ;; would't be nearly so difficult.
              (setq micro-step (make-iv-cluster
                                (domain-of macro-step)
                                (intern
                                 (format nil "u~A"
					 (weyli::symbol-of macro-step)))
                                '(h)))
              (setf (getf micro-step :macro-step) macro-step)
              (setf (getf macro-step :micro-step) micro-step)
              (push macro-step macro-steps)
              (push micro-step micro-steps))

              (setq new-args (copy-list (args-of exp)))
              (setf (cl::nth (second discretization) new-args)
                    (disc-var-of micro-step))
              (push (list exp :discrete-expression
                          (apply (first discretization) new-args))
                    env)
              ))
    
    (cond ((null micro-steps)
           form)
          (t 
           ;; Now create the bindings.  For each micro-step, 
           (loop for var in micro-steps
                 for macro-step = (getf var :macro-step)
                 do (push (list* macro-step 
                                 (list (cont-var-of var) :var :var)
                                 (list (disc-var-of var) 0
                                       (wrm::type nil (get-rational-integers)))
                                 (mapcar 
                                  #'(lambda (v) 
                                      (list v (zero (get-real-numbers))
                                            (wrm::type nil (get-real-numbers))))
                                  (steps-of var)))
                          *code-bindings*))
           
           (values
            (make-sequence
             (loop for term in (terms-of form)
		   for new-term = (walk-form-internal term :eval env)
		   with terms
		   do (setq terms (if (listp new-term)
				      (nreconc new-term terms)
				      (cons new-term terms)))
                   finally (return (nreverse terms))))
            :done)))))

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
				       (delete (first td (cl:third td2)))))
			(setq repeat? t)
			(return t))
		   finally (setq repeat? nil)))
    *type-dependencies*))




