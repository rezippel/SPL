(in-package 'wrmi)

(defvar *type-dependencies* nil)

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
