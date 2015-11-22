;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;			Finite Element Method
;;; ===========================================================================
;;; (c) copyright 1995 Cornell University

;;; $id: upolynomial.lisp,v 2.11 1992/05/15 15:55:08 rz exp $

(in-package "WRMI")

;; The following routine computes the primitive polynomial shape
;; funtions of degree d when given the a list of n points that delimit
;; a simplex.  It returns a general-expressions for the shape
;; polynomials, the variables that are used to represent the shape
;; functions, and the vertices at whcih the shape functions take on
;; value 1 (in barycentric coordinates).

(defvar *poly-shape-funs* (make-hash-table :test #'equal))

(defvar *boundary-points* nil
  "Points on the boundary of mesh that is being discretized")

;; The list of vertices is also the list of exponent vectors to be
;; used in the interpolating polynomial.
(defun fem-poly-shape-funs (d n &optional (domain *general*))
  (let* ((key (list d n domain))
	 (vals (gethash key *poly-shape-funs*))) 
    (cond (vals (values (first vals) (second vals) (third vals)))
	  (t (setq vals (%fem-poly-shape-funs d n domain))
	     (setf (gethash key *poly-shape-funs*) vals)
	     (values (first vals) (second vals) (third vals))))))

(defun %fem-poly-shape-funs (d n domain)
  (let* ((expts (weyli::degree-partition n d))
	 ;; Vertices in barycentric coordinates
	 (vertices-bary (loop for vertex in expts
			      collect (loop for v in vertex
					    collect (/ v d))))
	 (vertices (mapcar #'rest vertices-bary))
	 (vals (loop for i below (length expts)
		     collect 0))
	 (vars (loop for i below (1- n)
		     collect (coerce (intern (format nil "X~D" i)) domain)))
	 polys)
	(setq expts (mapcar #'rest expts))
	(loop for i below (length vals)
	      do (setf (cl:first (nthcdr i vals)) 1)
		 (push (interpolate vars vertices vals)
		       polys)
		 (setf (cl:first (nthcdr i vals)) 0))
	(list (reverse polys) vars vertices-bary)))

(defun canonical-simplex-array (domain dim)
  (let ((array (make-array (list dim dim)))
	(one (one domain))
	(zero (zero domain)))
    (loop for i below dim do
      (loop for j below dim do
	(setf (aref array i j)
	      (if (or (= j (1- dim))
		      (= (+ i j) (1- dim)))
		    one
		  zero))))
    array))		    

(defun map-to-barycentric (pts vars)
  (let* ((dim+1 (length pts))
	(array (make-array (list dim+1 dim+1)))
	(coef-domain (weyli::coefficient-domain-of (domain-of (first pts)))))
    (loop for pt in pts
	  for i upfrom 0
	  do (setf (aref array i (1- dim+1)) (one coef-domain)) 
	     (loop for j below (1- dim+1)
		   do (setf (aref array i j) (ref pt j))))
    (setq array (weyli::times-array (weyli::invert-array coef-domain array)
				    dim+1 dim+1
				    (canonical-simplex-array coef-domain dim+1)
				    dim+1))
    (loop for j below (1- dim+1)
	  collect (loop for i below dim+1
			for v in vars
			with ans = (aref array (1- dim+1) j)
			do (setq ans (+ ans (* (aref array i j) v)))
			finally (return ans)))))

;; Given a simplex and a degree, this routine returns a list of
;; pairs. The first element of the list is a point contained in the
;; simplex, and the second argument is an applicable function whose
;; value is one a that point an which vanishes at every other
;; interpolation point.

(defun list-inner-product (a-list b-list)
  (loop with ans = (* (first a-list) (first b-list))
	for a in (rest a-list)
	for b in (rest b-list)
	do (setq ans (+ ans (* a b)))
	finally (return ans)))

(defmethod fem-shape-functs ((simp weyli::simplex) degree)
  (let* ((dim (dimension simp))
	 (arglist (loop for i below dim
			collect (coerce (intern (format nil "V.~D" (1+ i)))
					*general*)))
	 (simp-vertices (weyli::vertices-of simp))
	 (nvars (map-to-barycentric simp-vertices arglist)))

	(setq simp-vertices (cons (first simp-vertices)
			      (reverse (rest simp-vertices))))
    (flet ((subsimp-on-boundary? (vertices)
	     (loop for v in vertices
		   for pt in simp-vertices
		   do (when (and (not (0? v))
				 (null (member pt *boundary-points*
					       :test #'weyli::binary=)))
			(return nil))
		   finally (return t))))
      (multiple-value-bind (polys vars vertices)
	  (fem-poly-shape-funs degree (1+ (dimension simp)))

	(loop for p in polys
	      for vertex in vertices
	      collect
	      (loop for v in vars
		    for nv in nvars
		    do (setq p (substitute nv v p))
		    finally (return
			      (list (list-inner-product simp-vertices vertex)
				    (weyli::make-app-function
				      arglist (weyli::expand p))
				    (subsimp-on-boundary? vertex)))))))))

(defun test-shape-funs (simp degree)
  (let* ((shapes (fem-shape-functs simp degree))
	 (points (mapcar #'first shapes))
	 (dim (dimension (domain-of (first points)))))
    (flet ((test-shape (fun pt)
	     (apply fun (loop for i below dim collect (ref pt i)))))
      (loop for (vertex func) in shapes do
	(loop for pt in points 
	      do (unless (= (test-shape func pt)
			    (if (eql pt vertex) 1 0))
		   (print (list (list  func vertex) pt
				'= (test-shape func pt)))))))))
		      
;; Shape-funs is a hash table indexed by points that contains a list
;; of pairs consisting of the shape functions that don't vanish and
;; the simplex over which they are defined.



(defmethod make-fem-basis-functions ((cc weyli::cell-complex) degree)
  (let (shape-funs
	chain-module-dimension
	chain-module
	coeff-field
	basis-funs
	test-fun unknowns)
    (weyli::map-over-maximal-cells (cell) cc
      (if (null chain-module-dimension)
	  (setf chain-module-dimension (dimension cell))
	  (when (not (eql chain-module-dimension (dimension cell)))
	    (error "Not all maximal cells are of the same dimension")))
      ;; boundary? indicates whether this is a boundary point or not. 
      (loop for (pt fun boundary?) in (fem-shape-functs cell degree)
	    for list = (assoc pt shape-funs :test #'weyli::binary=)
	    do (if (null list)
		   (push (list pt boundary? (cons cell fun))
			 shape-funs)
		   (push (cons cell fun) (cl:rest (cl:rest list))))))

    (setq coeff-field *general*)
    (setq chain-module (get-chain-module cc chain-module-dimension
					 coeff-field))

    (setq test-fun (zero chain-module))

    (loop for (pt boundary? . funs) in shape-funs
	  for fun = (weyli::make-chain chain-module funs)
	  with i = 0 and var 
	  do (push (list pt boundary? fun) basis-funs)
	     (setq var (coerce (intern (format nil "C~D" (incf i)))
			       coeff-field))
	     (push var unknowns)
	     (setq test-fun (+ (* var fun) test-fun)))

    (values basis-funs test-fun unknowns)))

(defmethod triangle-area ((p1 point) (p2 point) (p3 point))
  (let ((domain (domain-of p1)))
    (unless (and (eql domain (domain-of p2))
		 (eql domain (domain-of p3)))
      (error "Points must be in same domain"))
    (cond ((= (dimension domain) 2)
	   (/ (- (+ (* (ref p1 0) (ref p2 1)) (* (ref p2 0) (ref p3 1))
		    (* (ref p3 0) (ref p1 1)))
		 (+ (* (ref p1 0) (ref p3 1)) (* (ref p2 0) (ref p1 1))
		    (* (ref p3 0) (ref p2 1))))
	      2))
	  (t (error "Not implmented")))))

;; FIXTHIS:: The first method here should be moved to lisp-support,
;; the second to sets.lisp.  The lisp function length needs to be
;; overloaded.  This means changing all current occurences of length
;; in weyl to cl:length.

(defmethod llength ((w sequence))
  (cl:length w))

(defmethod llength ((w tuple))
  (cl::length (weyli::tuple-value w)))

(defmethod ratio? ((v1 tuple) (v2 tuple))
  (let ((d (llength v1)))
    (labels ((test (i)
	       (cond ((not (cl:< i d))
		      t)
		     ((0? (ref v2 i))
		      (cond ((0? (ref v1 i))
			     (test (1+ i)))
			    (t (loop for i upfrom i below d
				     do (unless (0? (ref v2 i))
					  (return nil))
				     finally (return t)))))
		     (t (loop with ratio = (/ (ref v1 0) (ref v2 0))
			      for i upfrom 1 below d
			      do (unless (= ratio (/ (ref v1 i) (ref v2 i)))
				   (return nil))
			      finally (return t))))))
       (and (eql d (llength v2))
	    (test 0)))))       

(defmethod contained-in? ((p point) (simplex simplex))
  (let ((domain (domain-of p)))
    (cond ((eql (dimension simplex) 1)
	   (let* ((points (weyli::vertices-of simplex))
		  (p0 (first points))
		  (p1 (second points)))
	     (ratio? (- p0 p) (- p p1))))
	  ((eql (dimension domain) 2)
	   (let* ((points (weyli::vertices-of simplex))
		 (p1 (first points))
		 (p2 (second points))
		 (p3 (third points))
		 a1 a2 a3)
	     (setq a1 (triangle-area p p1 p2))
	     (setq a2 (triangle-area p p2 p3))
	     (setq a3 (triangle-area p p3 p1))
	     (or (and (not (plusp a1)) (not (plusp a2)) (not (plusp a3)))
		 (and (not (minusp a1)) (not (minusp a2)) (not (minusp a3)))))))))

(defmethod evaluate-at ((c weyli::chain) (pt weyli::point))
  (loop for (simplex . coef) in (weyli::chain-terms-of c)
	do (when (contained-in? pt simplex)
	     (return
	       (apply coef (loop for i below (dimension (domain-of pt))
				 collect (ref pt i)))))
	finally (return (zero *general*))))

;; 7 point quadrature integration of (X**M)(Y**N)(DA)
;; on AN ARBITRARY TRIANGLE (area coord)
;;   Refer  Meek,  Matrix Structural Analysis

(defvar *triangint-weights*
    (make-array 7 :element-type 'float
		:initial-contents '(0.22500000 0.13239415 0.13239415
				    0.13239415 0.12593918 0.12593918
				    0.12593918)))
(defvar *triangint-A1*
        (make-array 7 :element-type 'float
		    :initial-contents '(0.33333333 0.47014206 0.47014206
					0.05971587 0.10128651 0.79742699
					0.10128651)))

(defvar *triangint-A2*
        (make-array 7 :element-type 'float
		    :initial-contents '(0.33333333 0.05971587 0.47014206
					0.47014206 0.10128651 0.10128651
					0.79742699)))

(defvar *triangint-A3*
        (make-array 7 :element-type 'float
		    :initial-contents '(0.33333333 0.47014206 0.05971587
					0.47014206 0.79742699 0.10128651
					0.10128651)))

;; FIXTHIS:  Add convert-to-lisp-number to manual. 

(defmethod triang-int ((simplex weyli::simplex) m n)
  (let* ((points (weyli::vertices-of simplex))
	 (x1 (float (convert-to-lisp-number (ref (first points) 0))))
	 (x2 (float (convert-to-lisp-number (ref (second points) 0))))
	 (x3 (float (convert-to-lisp-number (ref (third points) 0))))
	 (y1 (float (convert-to-lisp-number (ref (first points) 1))))
	 (y2 (float (convert-to-lisp-number (ref (second points) 1))))
	 (y3 (float (convert-to-lisp-number (ref (third points) 1))))
	 (value 0.0))
	(loop for i fixnum below 7
	      for xp = (cl:+ (cl:* x1 (aref *triangint-A1* i))
			     (cl:* x2 (aref *triangint-A2* i))
			     (cl:* x3 (aref *triangint-A3* i)))
	      for yp = (cl:+ (cl:* y1 (aref *triangint-A1* i))
			     (cl:* y2 (aref *triangint-A2* i))
			     (cl:* y3 (aref *triangint-A3* i)))
	      do (setq value (cl:+ value
				(cl:* (cl:expt xp m) (cl:expt yp n)
				      (aref *triangint-weights* i)))))
	(cl:* value
	      ;; Area
	      0.5
	      (cl:+ (cl:* x1 (cl:- y2 y3))
		    (cl:* x2 (cl:- y3 y1))
		    (cl:* x3 (cl:- y1 y2))))))

(setq weyli::*fem-kludge* t)

(defmethod weyli::integral ((ch weyli::chain) &rest ignore)
  (declare (ignore ignore))
  (loop with sum = (zero *general*)
	for (simp . coef) in (weyli::chain-terms-of ch)
	do (setq sum (+ sum (simplex-integral coef simp)))
	finally (return sum)))

(defmethod simplex-integral ((fun weyli::applicable-function)
			     (simp weyli::simplex))
  (let ((simp-dim (dimension simp)))
    (unless (eql (dimension simp) (length (weyli::bound-vars-of fun)))
      (error "Function not well formed"))
    (cond ((eql simp-dim 0)
	   (funcall fun))
	  ((eql simp-dim 1)
	   (- (apply fun (list-of-elements (second (vertices-of simp))))
	      (apply fun (list-of-elements (first (vertices-of simp))))))
	  ((eql simp-dim 2)
	   (simplex-integral2 (weyli::bound-vars-of fun)
			      (if (ge-plus? (weyli::body-of fun))
				  (weyli::terms-of (weyli::body-of fun))
				  (list (weyli::body-of fun)))
			      simp))
	  (t (error "Can't integrate over ~D dimensional simplices yet."
		    simp-dim)))))

(defun simplex-integral2 (vars term-list simp)
  (let ((sum (zero *general*))
	(one (one *general*))
	coef m n)
    (labels ((parse-term (term)
	       (cond ((number? term)
		      (setq coef (* coef term)))
		     ((weyli::ge-atom? term)
		      (cond ((eql term (first vars))
			     (setq m (+ m 1)))
			    ((eql term (second vars))
			     (setq n (+ n 1)))
			    (t (setq coef (* term coef)))))
		     ((ge-times? term)
		      (mapcar #'parse-term (weyli::terms-of term)))
		     ((ge-expt? term)
		      (cond ((eql (weyli::base-of term) (first vars))
			     (setq m (+ (weyli::exponent-of term) m)))
			    ((eql (weyli::base-of term) (second vars))
			     (setq m (+ (weyli::exponent-of term) n)))
			    (t (setq coef (* term coef)))))
		     (t (setq coef (* term coef))))))
	(loop for term in term-list
	      do (setq coef one m 0 n 0)
		 (parse-term term)
		 (setq m (convert-to-lisp-number m)
		       n (convert-to-lisp-number n))
		 (setq sum (+ sum (* coef (triang-int simp m n))))))
    sum))



;; The following coercions shouldn't be necessary, but unfortunately,
;; the mesher returns simplices with points in all sorts of wierd
;; spaces.
(defun cleanup-mesh (mesh)
  (let ((cells))
    (weyli::map-over-maximal-cells (cell) mesh
      (push (apply #'make-simplex
		   (mapcar #'(lambda (pt) (coerce pt (weyli::home-of cell)))
			   (vertices-of cell)))
	    cells))
    (apply #'make-cell-complex cells)))

(defun boundary-simplex? (simp)
  (loop for vertex in (vertices-of simp)
	do (unless (member vertex *boundary-points* :test #'weyli::binary=)
	     (return nil))
	finally (return t)))

#+ignore  ;; Incomplete
(defun galerkin-method (mesh equations fun &optional (degree 1))
  (let ((weyli::*fem-kludge* t)
	*boundary-points* constraints)
    (setq mesh (cleanup-mesh mesh))

    (loop for (simp) in (weyli::chain-terms-of (boundary mesh))
	  do nil #+ignore
	     (map-over-faces simp (pt 0)
	       (pushnew (first pt) *boundary-points* :test #'weyli::binary=)))

    (unless (listp equations)
      (setq equations (list equations)))

    (setq equations (loop for eq in equations
			  collect (if (ge-eqn=? eq)
				      (- (lhs-of eq) (rhs-of eq))
				      eq)))

    (multiple-value-bind (basis test vars)
	(make-fem-basis-functions mesh degree)
      (loop for equation in equations
	    for error = (substitute test fun equation)
	    do  (loop for (pt boundary? base) in basis
		     do (push
			  (if boundary?
			      (- (evaluate-at test pt) (ref pt 0))
			      (integral (inner-product base error)))
			  constraints)))

      (values constraints vars test))))

(defun message (&rest args)
  (apply #'format args))

(defvar *degree* nil
  "Degree of interpolation in FEM")

(defun galerkin-transform (p *degree*)
  (let ((meshes (find-meshes p)))
    (cond ((null meshes)
	   (message "Nothing to discretize"))
	  ((rest (rest meshes))
	   (error "To many meshes to discretize: ~S" meshes)))
    
    ;; The following certainly needs to be more generic
    (discretize-mesh (value-of (first meshes))
		     '(:size-list (weyli::circle-boundary 1.0)))

    (walk-form (mark-constrained-vars p '(:constrain) t)
      #'galerkin-transform-walker)))

(defun galerkin-transform-walker (form context env)
  (declare (ignore context))
  (cond ((typep form 'binding) 
	 (bvp-advance-binding form env))
	((typep form 'application)
	 (bvp-advance-application form env))
	((ge-application? form)
	 (let (disc-fun)
	   (setq disc-fun
		 (env-lookup (weyli::funct-of form) :discrete-function env))
	   (if disc-fun
	       (values (apply (first disc-fun) (args-of form))
		       :done)
	       form)))
	(t form)))

(defun bvp-advance-binding (form env)
  (declare (ignore env))
  (let ((type (type-of form)))
    (flet ((discrete-form (form dform) 
	     (push (list (var-of form) :discrete-function dform)
		   *new-envs*)
	     form)) 
      (cond ((and (bi-type? type)
		  (typep (domain-of type) 'function-space))
	     (discrete-form form 
			    (apply #'discretize-function (var-of form)
				   (loop for i below (nargs-of (var-of form))
					 collect i))))
	    (t form)))))

(defun bvp-advance-application (form env)
  (let ((constraints nil))
    (cond ((and (touch? form)
		(null (rest (args-of form))) 
		(ge-function? (first (args-of form)))
		(setq constraints
		      (env-constraints env (first (args-of form)))))
	   (cond ((and (null (rest constraints))
		       (typep (first constraints) 'universal-quantified-set)
		       (null (rest (bound-vars-of (first constraints)))))
		  (multiple-value-bind (equations vars test-fun)
		      (galerkin-method
			(second (first (bound-vars-of (first constraints))))
			(exprs-of (first constraints))
			(first (args-of form)) *degree*)
		    (loop for var in vars
			  do (push (make-binding var nil (get-real-numbers))
				   *new-bindings*))
		    (push (make-binding
			    (first (env-lookup (first (args-of form))
					       :discrete-function env))
			    (galerkin-transform-test-function test-fun)
			    (env-lookup (first (args-of form))
					:type env))
			  *new-bindings*)
		    (setq equations
			  (loop for eq in equations
				collect (if (ge-eqn=? eq) eq
					    (eqn= eq 0))))
		    (values (make-constrain vars equations
					    (make-touch vars))
			    :done)))
		 (t form)))
	  (t form))))



(defun galerkin-transform-test-function (test-fun)
  (let* ((terms (weyli::chain-terms-of test-fun))
	 (vars (weyli::bound-vars-of (rest (first terms)))))
    (labels ((transform (terms) 
	       (when terms
		 (make-conditional
		   (make-application 'inside?
				     (append vars (list (first (first terms)))))
		   (weyli::body-of (rest (first terms)))
		   (transform (rest terms))))))
      (make-function vars (transform terms)))))

(defun find-meshes (p)
  (let ((meshes))
    (walk-form p #'(lambda (form context env)
                     (declare (ignore context env))
                     (when (and (typep form 'binding)
                                (typep (value-of form) 'weyli::mesh))
                       (push form meshes))
                     form))
    meshes))

(defun discretize-mesh (mesh control-args)
  (setq *mesh* mesh)
  (apply #'weyli::mesh-control *mesh* control-args)
  (weyli::all-improve *mesh*))


#|Test problem |

(defun fem-setup ()
  (weyli::init-draw :window-size '(250 250))
  (setq eq1 (+ (deriv (funct u 'x 'y) 'x) (deriv (funct u 'x 'y) 'y)))
  (setq eq2 (+ (deriv (funct u 'x 'y) 'x 'x) (deriv (funct u 'x 'y) 'y 'y)))
  (setq eq3 (+ (* (funct u 'x 'y) (deriv (funct u 'x 'y) 'x))
	       (deriv (funct u 'x 'y) 'y)))
  (setq u (funct-of (funct u 'x 'y)))

  (setq c (weyli::circle :size-list '(weyli::circle-boundary 1.0))))



  	  


(defun setup ()
  (setq sp (get-euclidean-space 2))
  (setq a (make-point sp -1 0) b (make-point sp 1 0)
	c (make-point sp 0 1) d (make-point sp 0 -1))
  (setq s1 (make-simplex a b c)
	s2 (make-simplex b a d))
  (setq cc (weyli::make-cell-complex s1 s2)))

(defun setup2 ()
  (setq sp (get-euclidean-space 2))
  (setq pt00 (make-point sp 0 0) pt01 (make-point sp 0 1)
	pt02 (make-point sp 0 2) pt10 (make-point sp 1 0)
	pt11 (make-point sp 1 1) pt12 (make-point sp 1 2)
	pt20 (make-point sp 2 0) pt21 (make-point sp 2 1)
	pt22 (make-point sp 2 2))
  (setq s1 (make-simplex pt00 pt10 pt11)
	s2 (make-simplex pt00 pt01 pt11)
	s3 (make-simplex pt10 pt20 pt21)
	s4 (make-simplex pt10 pt11 pt21))
  (setq s5 (make-simplex pt01 pt11 pt12)
	s6 (make-simplex pt01 pt02 pt12)
	s7 (make-simplex pt11 pt21 pt22)
	s8 (make-simplex pt11 pt12 pt22))
  (setq cc (weyli::make-cell-complex s1 s2 s3 s4 s5 s6 s7 s8)))

(make-fem-basis-functions cc 1)

||#
