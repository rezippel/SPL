;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		 General Purpose Code Walker
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University

(in-package "WRMI")

;; The following is a list of the basic forms in SPL
;;  application:  used for regular functions
;;  conditional:  (if <pred> <succeed> <fail>)
;;  sequence:     (sequence <label> <term> <term> ...)
;;  label:        <label>
;;  goto:         (go <label>)
;;  assignment:   (set! <foo> <bar>)
;;  bind:         (let <bindings> <term>)
;;                (let* <bindings> <term>)    ;; handled by parser
;;                (letrec <bindings> <term>)  ;; handled by parser
;;  binding       (<var> <type> <value>)
;;  function:     (lambda <args> <body>)
;;  program       (defprogram <name> <args> . <body>)

;;  constrain:    (constrain (<mvar> ...) (<constrain> ...) <term>)
;;  invariant:    (invariant (<mvar> ...) (<constrain> ...) <term>)
;;  minimize:     (minimize (<mvar> ...) (<math expression> ...) <term>)
;;  maximize:     (maximize (<mvar> ...) (<math expression> ...) <term>)




(defclass basic-term (weyli::has-property-list)
     ())

(defmethod print-object ((term basic-term) stream)
  (princ (convert-to-sexp term) stream))

(defmethod convert-to-sexp (term)
  term)

;; A term is something I haven't been able to classify yet.
(defclass term (basic-term)
     ((value :initarg :value
	     :accessor value-of)))

(defun make-term (value)
  (make-instance 'term :value value))

(defmethod convert-to-sexp ((term term))
  (if (null (value-of term)) "()"
      (value-of term)))

;; For debugging purposes, we timestamp every variable and, under user
;; control, include the timestamp in the print name of the variable.

(defvar *variable-counter* 0)
(defvar *include-variable-counter* nil)

;; The order that the sub-classes are specified ensures that the
;; print-object methods of ge-variable is used instead of the
;; basic-term one.
(defclass variable (weyli::ge-variable basic-term)
     ((timestamp :initform (incf *variable-counter*)
		 :accessor timestamp-of)))

(defmethod print-object :around ((var variable) stream)
  (if *include-variable-counter*
      (format stream "~A.~D"
	      (or (weyli::string-of var) (weyli::symbol-of var))
	      (timestamp-of var))
      (call-next-method var stream)))

(defun make-variable (name)
  (make-instance 'variable :domain *general* :symbol name))

(defmethod first ((v variable))
  (error "FIRST -- illegal operation on ~S" v))

(defmethod second ((v variable))
  (error "SECOND -- illegal operation on ~S" v))

(defmethod third ((v variable))
  (error "THIRD -- illegal operation on ~S" v))

(defmethod NTH ((n integer) (v variable))
  (error "NTH -- illegal operation on ~S" v))

(defmethod REST ((v variable))
  (error "REST -- illegal operation on ~S" v))

(defmethod NTHCDR ((n integer) (v variable))
  (error "NTHCDR -- illegal operation on ~S" v))

(defmethod convert-to-sexp ((v variable))
  (if (null *include-variable-counter*)
      (weyli::symbol-of v)
      (intern (format nil "~A.~D" (weyli::symbol-of v) (timestamp-of v)))))

(defmacro def-list-accessors (type arg &body prototype)
  `(progn
     (defmethod convert-to-sexp ((,(first arg) ,type))
       (list ,@(mapcar #'(lambda (x) `(convert-to-sexp ,x))
		       prototype)))
     (defmethod first ((,(first arg) ,type))
       ,(first prototype))
     (defmethod second ((,(first arg) ,type))
       ,(second prototype))
     (defmethod third ((,(first arg) ,type))
       ,(third prototype))
     (defmethod nth ((n integer) (,(first arg) ,type))
       (case n
	 ,@(loop for form in prototype
		 for i upfrom 0
		 collect (list i form))
	 (otherwise nil)))
     (defmethod rest ((,(first arg) ,type))
       (list ,@(rest prototype)))
     (defmethod nthcdr ((n integer) (,(first arg) ,type))
       (case n
	 (0 ,(first arg))
	 ,@(loop for form on (rest prototype)
		 for i upfrom 1
		 collect `(,i (list ,@form)))
	 (otherwise nil)))))

(defclass application (basic-term)
     ((func :initarg :func
	    :accessor funct-of)
      (args :initarg :args
	    :accessor args-of)))

(defun make-application (func args)
  (make-instance 'application :func func :args args))

(defmethod convert-to-sexp ((app application))
  (cons (funct-of app)
	(mapcar #'convert-to-sexp (args-of app))))

(defmethod first ((x application))
  (funct-of x))

(defmethod second ((x application))
  (cl:first (args-of x)))

(defmethod third ((x application))
  (cl:second (args-of x)))

(defmethod nth ((n integer) (x application))
  (if (zerop n) (funct-of x)
      (cl:nth (1- n) (args-of x))))

(defmethod rest ((x application))
  (args-of x))

(defmethod nthcdr ((n integer) (x application))
  (if (zerop n) x
      (cl:nth (1- n) (args-of x))))

(defclass conditional (basic-term)
     ((predicate :initarg :predicate
		 :accessor predicate-of)
      (succeed :initarg :succeed
	       :accessor succeed-of)
      (fail :initarg :fail
	    :accessor fail-of)))

(defun make-conditional (predicate succeed &optional fail)
  (make-instance 'conditional
		 :predicate predicate :succeed succeed :fail fail))

(def-list-accessors conditional (cond)
  'if (predicate-of cond) (succeed-of cond) (fail-of cond))

;; Sequences can contain goto's and tags.  At some point later on, the
;; we'll break the sequence up into basic blocks up and make the
;; goto's direct.

(defclass sequence (basic-term)
     ((terms :initarg :terms
	     :accessor terms-of)))

(defun make-sequence (terms)
  (let ((real-terms nil))
    (labels ((reduce-term (term)
	       (cond ((null term) nil)
		     ((typep term 'sequence)
		      (mapcar #'reduce-term (terms-of term)))
		     (t (push term real-terms)))))
     (mapcar #'reduce-term terms)
     (make-instance 'sequence :terms (nreverse real-terms)))))

(defmethod convert-to-sexp ((seq sequence))
  `(sequence ,@(mapcar #'convert-to-sexp (terms-of seq))))

(defmethod first ((x sequence))
  'sequence)

(defmethod second ((x sequence))
  (cl:first (terms-of x)))

(defmethod third ((x sequence))
  (cl:second (terms-of x)))

(defmethod nth ((n integer) (x sequence))
  (if (zerop n) 'tagbody
      (cl:nth (1- n) (terms-of x))))

(defmethod rest ((x sequence))
  (terms-of x))

(defmethod nthcdr ((n integer) (x sequence))
  (if (zerop n) x
      (cl:nth (1- n) (terms-of x))))

(defclass label (basic-term)
     ((name :initarg :name
	    :accessor name-of)))

(defun make-label (name)
  (make-instance 'label :name name))

(defmethod convert-to-sexp ((lab label))
  (name-of lab))

(defclass goto (basic-term)
     ((target :initarg :target
	      :accessor target-of)))

(defun make-goto (target)
  (make-instance 'goto :target target))

(def-list-accessors goto (g)
  'go (target-of g))

(defclass assignment (basic-term)
     ((location :initarg :location
		:accessor location-of)
      (value :initarg :value
	     :accessor value-of)))

(defun make-assignment (location value)
  (make-instance 'assignment :location location :value value))

(def-list-accessors assignment (x)
  'set! (location-of x) (value-of x))  

(defclass loop-int (basic-term)
     ((index :initarg :index
	     :accessor index-of)
      (interval :initarg :interval
		:accessor interval-of)
      (body :initarg :body
	    :accessor body-of)))

(defmethod convert-to-sexp ((l loop-int))
  (list 'loop-int (index-of l) 'in
 	(interval-of l)
	(convert-to-sexp (body-of l))))

(defun make-loop-int (v i b)
  (make-instance 'loop-int :index v
		           :interval i
			   :body b))

(defclass scopes-term (basic-term)
     ((term :initarg :term
	    :accessor term-of)))

(defclass constrain (scopes-term)
     ((vars :initarg :vars
	    :accessor vars-of)
      (constraints :initarg :constraints
		   :accessor constraints-of)))

(defun make-constrain (vars constraints term)
  (make-instance 'constrain
		 :vars vars :constraints constraints :term term))

(def-list-accessors constrain (c)
  'constrain (vars-of c) (constraints-of c)
  (term-of c))

(defclass extremize (scopes-term)
     ((vars :initarg :vars
	    :accessor vars-of)
      (expressions :initarg :expressions
		   :accessor expressions-of)))

(defclass minimize (extremize)
     ())

(defun make-minimize (vars constraints term)
  (make-instance 'minimize
		 :vars vars :expressions constraints :term term))

(def-list-accessors minimize (c)
  'minimize (vars-of c) (expressions-of c)
  (term-of c))

(defclass maximize (extremize)
     ())

(defun make-maximize (vars constraints term)
  (make-instance 'maximize
		 :vars vars :expressions constraints :term term))

(def-list-accessors maximize (c)
  'maximize (vars-of c) (expressions-of c)
  (term-of c))

;; Bindings are the actual variable/type/value triple.  A binding list
;; (e.g.  in bind) is a list of bindings.

(defclass binding (basic-term)     
     ((var :initarg :var
	   :accessor var-of)
      (type :initarg :type
	    :accessor type-of)
      (value :initarg :value
	     :accessor value-of)))

(def-list-accessors binding (b)
  (var-of b) (value-of b) (type-of b))

(defun make-binding (var value &optional type)
  (make-instance 'binding :var var :type (ensure-bi-type type) :value value))

(defclass bind (scopes-term)
     ((bindings :initarg :bindings
		:accessor bindings-of)))

(defun make-bind (bindings term)
  (make-instance 'bind :bindings bindings :term term))

(def-list-accessors bind (bind)
  'bind (bindings-of bind) (term-of bind))

(defclass function (scopes-term)
     ((args :initarg :args
	    :accessor args-of)))

(defun make-function (args term)
  (make-instance 'function :args args :term term))

(def-list-accessors function (p)
  'lambda (args-of p) (term-of p))

;; FIXTHIS: This is really stupid, but the reason program is not a
;; subtype of function is so that the pretty printer won't get
;; confused.  When program is a subtype, the pretty printer, for some
;; reason, prints it as if it were a function.
(defclass program (scopes-term)
     ((args :initarg :args
	    :accessor args-of)
      (name :initarg :name
	    :accessor name-of)))

(defun make-program (name args term)
  (make-instance 'program :name name :args args :term term))

(def-list-accessors program (p)
  'defprogram (name-of p) (args-of p) (term-of p))

;; This is the basic walker

;; Different contexts:
;;  :eval, :math, :bind, :declaration, :set

;; The environment is a list of items of the form:
;;  (<variable> :keyword . <information>)

(defmethod env-lookup (name type env)
  (loop for (var v-type . info) in env
	when (and (eql var name) (eql type v-type))
	  return info))

(defun env-equal (var name)
  (if (typep var 'variable)
      (string-equal (weyli::symbol-of var) name)
      (eql var name)))

(defmethod env-lookup ((name symbol) type env)
  (loop for (var v-type . info) in env
	when (and (eql type v-type)
		  (env-equal var name))
	  return info))

(defmethod env-lookup ((name weyli::general-expression) type env)
  (loop for (var v-type . info) in env
	when (and (eql type v-type)
		  (ge-equal var name))
	  return info))

(defmethod env-lookup ((name weyli::ge-variable) (type (eql :element)) env)
  (loop for (var v-type . info) in env
	when (and (eql v-type :element)
		  (if (typep var 'iv-cluster)
		      (or (eql name (disc-var-of var))
			  (eql name (cont-var-of var)))
		      (eql var name)))
	  return (append info (list var))))

(defmacro walk-function (env)
  `(first (env-lookup 'walk-function :function ,env)))

;; Constraints often deal with more than one variable.  So the <var>
;; slot of constraint in the environment consists of a list of all the
;; variables that are constrained.  The ENV-CONSTRAINTS function
;; lookups up constraints properly in the environement.  It is passed
;; a list of variables and returns two values: (1) a list of all the
;; constraints that the variables satisfy, and (2) a list of all the
;; variables that are involved in the constraints.

(defun env-constraints (env &rest vars)
  (let (constraints new-vars)
    (loop for (var v-type info) in env
	  when (and (eql v-type :constraint) 
		    (intersection var vars :test #'ge-equal))
	    do (loop for eq in info
		     do (unless (member eq constraints :test #'eql)
			  (push eq constraints)))
	       (loop for v in var
		     do (unless (member v new-vars :test #'ge-equal)
			  (push v new-vars))))
    (values constraints new-vars)))

(defmethod constraint-type ((form constrain))
  :constraint)

(defmethod constraint-type ((form minimize))
  :minimize)

(defmethod constraint-type ((form maximize))
  :maximize)

(defun env-extremize (env types &rest vars)
  (when (atom types)
    (setq types (list types)))
  (let (constraints new-vars)
    (loop for (var v-type info) in env
	  when (and (member v-type types) 
		    (intersection var vars :test #'ge-equal))
	    do (loop for eq in info
		     do (unless (member eq constraints :test #'eql)
			  (push eq constraints)))
	       (loop for v in var
		     do (unless (member v new-vars :test #'ge-equal)
			  (push v new-vars))))
    (values constraints new-vars)))

(defmacro environment-bind (env bindings &body body)
  (unless (symbolp env)
    (error "Expected ~A to be a symbol in ENVIRONMENT-BIND"
	   env))
  `(let ((,env ,env))
     ,@(loop for (var value) in bindings
	    collect `(push (list ',var :function ,value) ,env))
     ,@body))

(defvar *walk-form-simplify-expressions-p* t
  "The walker should simplify Weyl expressions after they are transformed.")

(defun walk-form (form walk-function &key (context :eval) (env nil))
 (unless walk-function
   (setf walk-function #'(lambda (form context env)
			   (declare (ignore context env))
			   form)))
  (environment-bind env ((walk-function walk-function))
    (walk-form-internal form context env)))

;; Walk-subform is the same is walk-form except that the walk-function
;; provided is not made active until we get to the indicated subform.

(defun walk-subform (form subform &key (context :eval) (env nil)
			  (walk-function #'(lambda (form context env)
					     (declare (ignore context env))
					     form)))
  (walk-form form 
	     #'(lambda (form context env)
	         (cond ((eql form subform)
		        (walk-form form walk-function :context context :env env))
		       (t form)))
	     :context context :env env))

;; After applying the user's walk-function, there are three different
;; things that can be done.  What is to be done is indicated by the
;; second value returned by the walk-function.

;;  nil       ==>  form has changed try again.
;;  :recurse  ==>  finished at this level, descend into the subnodes.
;;  :done     ==>  completely finished with expression.
;; (For backward compatibility, T = :done.)

(defun walk-form-internal (form context env)
  ;; Apply the user's function to the form
  (multiple-value-bind (new-form continue-state)
      (funcall (walk-function env) form context env)
    (cond ((or (eql continue-state :done) ; The user says we're finished
	       (eql continue-state t))
	   new-form)
	  ;; The user is finished with this level of the expression.
	  ;; Now recurse. Recursion is handled for each expression in
	  ;; a different manner.
	  ((or (eql continue-state :recurse)
	       (eq form new-form))
	   (walk-expression new-form context env))		
	  ;; The user made some transformation of the form.  Apply the
	  ;; user's function again.
	  (t
	    (walk-form-internal new-form context env)))))

;; The following includes atoms. 
(defmethod walk-expression (form context env)
  (declare (ignore context env))
  form)

(defun check-list-eql (a b)
  (loop for x in a
        for y in b
	unless (eq x y) do (return b)
	  finally (return a)))

(defmacro walk-arglist (arglist env)
  `(loop for var in ,arglist
	 do (cond ((atom var)
		   (push (list var :bind nil) ,env))
		  (t (push (list (first var) :bind nil) ,env)
		     (push (list (first var) :type (second var))
				 ,env)))))

(defmethod walk-expression ((form program) context env)
  (unless (eql context :eval)
    (error "Using DEFPROGRAM in context: ~A" context))
  (walk-arglist (args-of form) env)
  (let ((body (walk-form-internal (term-of form) :eval env)))
    (if (eql body (term-of form)) form
	(make-program (name-of form) (args-of form)
		      body))))

(defmethod walk-expression ((form application) context env)
  (unless (eql context :eval)
    (error "Using an APPLICATION in context: ~A" context))
  (let ((args (check-list-eql
		(args-of form)
		(mapcar #'(lambda (x)
			    (walk-form-internal x :eval env))
			(args-of form))))
	func)
    (setq func (walk-form-internal (funct-of form) :function env))
    (if (and (eql args (args-of form))
	     (eql func (funct-of form)))
	form
	(make-application func args))))

(defmethod walk-expression ((form conditional) context env)
  (unless (eql context :eval)
    (error "Using a CONDITIONAL in context: ~A" context))
  (let ((predicate (walk-form-internal (predicate-of form) :eval env))
	(succeed  (walk-form-internal (succeed-of form) :eval env))
	(fail  (walk-form-internal (fail-of form) :eval env)))
    (if (and (eql predicate (predicate-of form))
	     (eql succeed (succeed-of form))
	     (eql fail (fail-of form)))
	form
	(make-conditional predicate succeed fail))))

(defmethod walk-expression ((form assignment) context env)
  (unless (eql context :eval)
    (error "Using ASSIGNMENT in context: ~A" context))
  (let ((value (walk-form-internal (value-of form) context env))
        (loc (walk-form-internal (location-of form) :set env)))
    (if (and (eql value (value-of form))
             (eql loc (location-of form)))
        form
        (make-assignment loc value))))

(defmethod walk-expression ((form sequence) context env)
  (unless (eql context :eval)
    (error "Using SEQUENCE in context: ~A" context))
  (let ((terms (check-list-eql
		 (terms-of form)
		 (mapcar #'(lambda (x)
			     (cond ((typep x 'label)
				    (walk-form-internal x :label env))
				   (t (walk-form-internal x :eval env))))
			 (terms-of form)))))
    (if (eql terms (terms-of form))
	form
	(make-sequence terms))))

(defmethod walk-expression ((form loop-int) context env)
  (unless (equal context :eval)
    (error "Using LOOP-INT in context: ~A" context))
  (let ((newstart (walk-form-internal (weyli::start-of (interval-of form))
				      :eval env))
	(newend (walk-form-internal (weyli::end-of (interval-of form))
				    :eval env))
	(newbody (walk-form-internal (body-of form)
				     :eval env)))
    (if (and (eql newstart (weyli::start-of (interval-of form)))
	     (eql newend (weyli::end-of (interval-of form)))
	     (eql newbody (body-of form)))
	form
	(make-loop-int (index-of form)
		       (weyli::make-integer-sequence newstart newend)
		       newbody))))

(defmethod walk-expression ((form label) context env)
  (declare (ignore context env)) #+ignore
  (unless (eql context :label)
    (error "Using LABEL in context: ~A" context))
  form)

(defmethod walk-expression ((form goto) context env)
  (declare (ignore env))
  (unless (eql context :eval)
    (error "Using GO in context: ~A" context))
  form)

(defmethod walk-expression ((form constrain) context env)
  (unless (eql context :eval)
    (error "Using CONSTRAIN in context: ~A" context))
  (let ((vars (vars-of form))
	(constraints 
	  (check-list-eql (constraints-of form)
			  (mapcar #'(lambda (x)
				      (walk-form-internal x :math env))
				  (constraints-of form))))
	term)
    (push (list vars :constraint constraints) env)
    (setq term (walk-form-internal (term-of form) :eval env))
    (if (and (eql vars (vars-of form))
	     (eql constraints (constraints-of form))
	     (eql term (term-of form)))
	form
	(make-constrain vars constraints term))))

(defmethod walk-expression ((form extremize) context env)
  (unless (eql context :eval)
    (error "Using ~S in context: ~A" (type-of form) context))
  (let ((vars (vars-of form))
	(expressions 
	  (check-list-eql (expressions-of form)
			  (mapcar #'(lambda (x)
				      (walk-form-internal x :math env))
				  (expressions-of form))))
	term)
    (push (list vars (constraint-type form) expressions) env)
    (setq term (walk-form-internal (term-of form) :eval env))
    (if (and (eql vars (vars-of form))
	     (eql expressions (expressions-of form))
	     (eql term (term-of form)))
	form
	(funcall (if (typep form 'minimize) #'make-minimize #'make-maximize)
                 vars expressions term))))

;; When some internal operation should cause information to be added
;; to the env, it saves it on the *new-envs*.
(defvar *new-envs* ())

(defvar *new-initial-terms* ())
(defvar *new-final-terms* ())

;; WALK-BINDINGS is used by walk functions that need to do 
;; something special with bindings

;; Notice that CHANGES the value of ENV.  Walking the bindings could
;; also cause new expressions to be inserted into the body of the of
;; statment that contains the BINDINGS. This is done by changing the
;; value of *NEW-INITIAL-TERMS* and *NEW-FINAL-TERMS*.

;; This is not as simple as it looks!
(defmacro walk-bindings (bindings env)
  `(let ((bindings
	 (check-list-eql
	   ,bindings
	   (let ((new-bindings nil)
		 (*new-envs* nil))
	     (loop for x in (bindings-of form)
		   for b = (walk-form-internal x :binding ,env)
		   do (cond ((typep b 'binding)
			     (push b new-bindings))
			    ((listp b)
			     (setq new-bindings (append b new-bindings)))
			    (t (error "Wrong type of object ~A" b))))
	     (setq ,env (nreconc *new-envs* ,env))
	     (reverse new-bindings))))) 
     (loop for b in bindings
	   do (when (not (null (value-of b)))
		(push (list (var-of b) :bind (value-of b))
		      ,env))
	      (when (not (null (type-of b)))
		(push (list (var-of b) :type (type-of b))
		      ,env)))
     bindings))

(defvar *new-bindings* ())

(defmethod walk-expression ((form bind) context env)
  (unless (eql context :eval)
    (error "Using BIND in context: ~A" context))
  (walk-bind-internal form env #'(lambda (term env)
				   (walk-form-internal term :eval env)))
  #+ignore
  (let (*new-initial-terms* *new-final-terms* *new-bindings*
	bindings term)
    (setq bindings (walk-bindings (bindings-of form) env))  
    
    (setq term (walk-form-internal 
                 (sequence-include-bind-terms (term-of form))
		 :eval env))

      (if (and (eql bindings (bindings-of form))
	       (eql term (term-of form))
	       (null *new-bindings*))
	form
	(make-bind (append bindings *new-bindings*) term))))

(defun walk-bind-internal (form env function)
  (let (*new-initial-terms* *new-final-terms* *new-bindings*
	bindings term)
    (setq bindings (walk-bindings (bindings-of form) env))  
    
    (setq term (funcall function 
			(sequence-include-bind-terms (term-of form))
			env))

    (if (and (eql bindings (bindings-of form))
	     (eql term (term-of form))
	     (null *new-bindings*))
	form
	(make-bind (append bindings (reverse *new-bindings*)) term))))

(defun sequence-include-bind-terms (term)  
  (if (or *new-initial-terms* *new-final-terms*)
    (make-sequence
     (append *new-initial-terms*
             (if (typep term 'sequence) (terms-of term)
               term)
             *new-final-terms*))
    term))

(defmethod walk-expression ((form weyli::ge-nary) context env)
  (let (new-terms changed?)
    (loop for arg in (terms-of form)
	  for new-form = (walk-form-internal arg context env)
	  do (push new-form new-terms)
	     (when (not (eq new-form arg))
	       (setq changed? t)))
    (when changed?
      (setq form (make-instance (class-of form)
				:domain (weyli:domain-of form)
				:terms (nreverse new-terms)))
      (possibly-simplify form))
    form))
	   
(defmethod walk-expression ((form weyli::ge-expt) context env)
  (let ((new-base (walk-form-internal (base-of form) context env))
	(new-expt (walk-form-internal (weyli:exponent-of form) context env)))
    (unless (and (eql new-base (base-of form))
		 (eql new-expt (exponent-of form)))
      (setq form 
	    (make-instance (class-of form)
			   :domain (domain-of form)
			   :base new-base :exp new-expt))
      (possibly-simplify form))
    form))

(defmethod walk-expression ((form weyli::ge-equation) context env)
  (let ((new-lhs (walk-form-internal (lhs-of form) context env))
	(new-rhs (walk-form-internal (rhs-of form) context env)))
    (unless (and (eql new-lhs (lhs-of form))
		 (eql new-rhs (rhs-of form)))
      (setq form (make-instance (class-of form)
				:domain (domain-of form)
				:lhs new-lhs :rhs new-rhs))
      (possibly-simplify form))
    form))

(defmethod walk-expression ((form universal-quantified-set) context env)
  (let ((bound-vars (bound-vars-of form))
	new-exprs changed?)
    (loop for (var set) in bound-vars
	  do (push (list var :element set) env))

    (loop for arg in (exprs-of form)
	  for new-arg = (walk-form-internal arg context env)
	  do (unless (eql new-arg arg)
	       (setq changed? t))
	     (push new-arg new-exprs))
    (when changed?
      (setq form (make-universal-quantified-set
		   (domain-of form) bound-vars (nreverse new-exprs))))
    (possibly-simplify form))
  form)

(defmethod walk-expression ((x weyli::ge-application) context env)
  #+ignore
  (unless (eql context :math)
    (error "Haven't implemented this case"))
  (let ((funct (walk-form-internal (funct-of x) :math-funct env))
        (args (loop for arg in (args-of x)
		    collect (walk-form-internal arg context env))))
    (if (and (eql funct (funct-of x))
             (eql args (args-of x)))
	x
	(apply #'weyli::make-ge-funct (domain-of x) funct args))))

;; We frequently want to write a walker that only applies to when a
;; certain condition holds.  The following special form makes it quite
;; easy to do this.
(defmacro def-conditional-walker (name args predicate &body body)
  `(defun ,name ,args
     (if ,predicate (progn ,@body) ,(first args))))
