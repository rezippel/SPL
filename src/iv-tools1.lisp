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




