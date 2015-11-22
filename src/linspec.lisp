;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;		      A Matrix program specializer Transformation
;;;                                      LINSPEC
;;; ===========================================================================
;;; (c) copyright 1995 Cornell University

(in-package "WRMI")

;; This transformation can take a program which contains matric computations
;; written for dense matrics and specializes them for regular sparsity.  In
;; particular, they can be specialized for sparsity in which the contraints
;; on index variables can be expressed in terms of linear inequalities.

;; The primary interface is via function sparsity-specialize which takes
;; 3 arguments: a program, an environment which has initial assertions
;; about program constants (e.g., N > M), and a sparsity specification which
;; describes the type of sparsity.

;; The macros in file spl/src/sparsity.lisp provide a set of common sparsity
;; specifications for triangular and banded matrices.  The defsimp macros
;; can be used to create other sparsity specifications.

(import 'weyli::ge-same-var)
(import 'weyli::ge-equation)
(import 'weyli::ge-or-numeric)
(import 'weyli::ge-application)
(import 'weyli::ge-variable)
(import 'weyli::ge-nary)
(import 'weyli::ge-expt)
(import 'weyli::ge-eqn>)
(import 'weyli::ge-eqn>=)

(import 'weyli::domain-of)
(import 'weyli::exponent-of)

(import 'weyli::ge-max-pair?)
(import 'weyli::ge-min-pair?)
(import 'weyli::eqn-complement)
(import 'weyli::canonicalize)
(import 'weyli::testineq)
(import 'weyli::interval-from-ineq)

(import 'weyli::integer-sequence)
(import 'weyli::make-integer-sequence)

(defmethod interval-intersect ((i1 integer-sequence) (i2 integer-sequence)
			       &optional env)
  (if (null env)
    (make-integer-sequence
      (max (weyli::start-of i1) (weyli::start-of i2))
      (min (weyli::end-of i1) (weyli::end-of i2)))
    (make-integer-sequence
      (simplify-max (max (weyli::start-of i1) (weyli::start-of i2))
		    env)
      (simplify-min (min (weyli::end-of i1) (weyli::end-of i2))
		    env))))

(defmethod simplify-interval ((i integer-sequence) env)
  (let ((mn (weyli::start-of i))
	(mx (weyli::end-of i)))
  (cond ((and (ge-max-pair? mn) (ge-min-pair? mx))
	 (make-integer-sequence (simplify-max mn env) (simplify-min mx env)))
	((ge-max-pair? mn)
	 (make-integer-sequence (simplify-max mn env) mx))
	((ge-min-pair? mx)
	 (make-integer-sequence mn (simplify-min mx env)))
        (t i))))	 

(defun print-lp-problem (stream obj constraints &optional int-vars)
  (format stream "max: ~A ;~%" obj)
  (loop for constraint in constraints
	do (format stream "~A;~%" constraint))
  (unless (null int-vars)
    (format stream "int ~A" (first int-vars))
    (loop for int-var in (cdr int-vars)
	  do (format stream ", ~A" int-var))
    (format stream ";~%")))

(defun env-vars (env)
  (loop for varintpair in env
	collect (car varintpair)))

(defmethod substringp (s1 s2)
  (declare (ignore s1)
	   (ignore s2))
  nil)

(defmethod substringp ((slist list) (s2 string))
  (loop for s1 in slist
	thereis (substringp s1 s2)))

(defmethod substringp ((s1 string) (s2 string))
  (let ((l (length s1)))
    (loop for i from 0 to (- (length s2) l)
	  thereis (string= s1 s2 :start2 i :end2 (+ i l)))))

;; stream-find takes a stream and either a single string or a list of strings.
;; It returns true if the string given (or one in the list, if given) appears
;; in the stream.  It is expected that the stream is at the beginning of a
;; file.  The stream will be left at the end of the file upon return.
(defun stream-find (stream s)
  (do ((line (read-line stream nil 'eof)
	     (read-line stream nil 'eof)))
      ((or (substringp s line)
	   (not (listen stream))
	   (eq line 'eof))
       (substringp s line))))

;; infeasible? utilizes a publicly available mixed linear integer
;; programming package (lpsolve), available through netlib, to disprove
;; the inequalities by constructing infeasible integer programming
;; problems.

;; Currently, it calls the lp solver as a separate process using
;; a file to obtain the results.  There are currently three ways that
;; it detects that the lp output indicates an infeasible problem:
;;   1) The word "infeasible" appears in the output
;;   2) The word "non-negative" appears in the error output -- This is
;;      an indication that the solver has determined that some variable
;;      must take on a negative value (which is infeasible by definition)
;;   3) The word "contradicts" appears in the error output -- This is
;;      an indication that the solver has found simple bounds constraints
;;      which contradict each other making some variable have no possible
;;      values.
;; Ideally, the lp solver would report these errors in a consistent way.
;; Instead, it tries to provide the intended user (a human) with additional
;; information regarding why the system in infeasible, resulting in an
;; awkward interface to this code.

;; Consequently, should the lp solver change, it is likely that modifications
;; will be needed here to correctly interpret the output of the lp program.

;; *tmp-file* is a temporary file which contains the output of the lp solver
(defvar *tmp-file* (make-pathname
                    :directory (pathname-directory (user-homedir-pathname))
                    :name ".lp_prob"))

;; *lp-solve-program* is a string which contains the full path and program
;; name of the lp solver that will be used.  

;(defvar *lp-solve-program* "~mardis/weyl/lp/lp_solve_2.0b3/lp_solve")
(defvar *lp-solve-program*
       (make-pathname
	 :directory (pathname-directory
	      (make::module-directory "lp" "lp_solve_2.0b3"))
	 :name "lp_solve"))

(defun infeasible? (var ineq-sys int-vars)
  (with-open-file (lp-prob *tmp-file* :direction :output
		  :if-exists :supersede
		  :if-does-not-exist :create)
    (print-lp-problem lp-prob var ineq-sys int-vars)
    (force-output lp-prob))
  (multiple-value-bind (lp-ans1 lp-err1)
      (user::run-program *lp-solve-program*
			 :input *tmp-file*
			 :output :stream
			 :error-output :stream
			 :wait nil)
    (with-open-stream (lp-ans lp-ans1)
      (with-open-stream (lp-err lp-err1)
	(peek-char t lp-ans)
        (or (stream-find lp-ans "infeasible")
	    (stream-find lp-err '("non-negative" "contradicts")))))))

;; Simplify routines
;; simplify-max, simplify-min

;; simplify-min must be given a ge-application of the min-pair function
;; simplify-max must be given a ge-application of the max-pair function

;; These routines attempt to simplify min and max expressions by proving
;; inequalities about the arguments to min-pair or max-pair.  The environment
;; contains the interval constraints on variables which appear in the
;; given expression.

(defmacro simplify-max3 (a11 a12 a2)
  `(let ((e11 ,a11)
	 (e12 ,a12)
	 (e2 ,a2))
     (let ((s (simplify-max (max e11 e2) env)))
       (if (ge-max-pair? s)
	   (max (simplify-max (max e12 e2) env) e11)
	   (simplify-max (max e12 s) env)))))

(defmacro simplify-min3 (a11 a12 a2)
  `(let ((e11 ,a11)
	 (e12 ,a12)
	 (e2 ,a2))
     (let ((s (simplify-min (min e11 e2) env)))
       (if (ge-min-pair? s)
	   (min (simplify-min (min e12 e2) env) e11)
	   (simplify-min (min e12 s) env)))))

(defmethod simplify-max (m env)
  (declare (ignore env))
  m)

(defmethod simplify-max ((m ge-application) env)
  (let ((arg1 (first (args-of m)))
	(arg2 (second (args-of m))))
    (cond ((ge-equal arg1 arg2)
	   arg1)
	  ((ge-max-pair? arg1)
	   (simplify-max3 (first (args-of arg1))
			   (second (args-of arg1))
			   arg2))
	  ((ge-max-pair? arg2)
	   (simplify-max3 (first (args-of arg2))
			   (second (args-of arg2))
			   arg1))
	  ((falsifiable? (eqn> arg1 arg2) env)
           arg2)
	  ((falsifiable? (eqn> arg2 arg1) env)
	   arg1)
	  ((and (number? arg1)
		(number? arg2))
	   (max arg1 arg2))
	  (t m))))

(defmethod simplify-min (m env)
  (declare (ignore env))
  m)

(defmethod simplify-min ((m ge-application) env)
  (let ((arg1 (first (args-of m)))
	(arg2 (second (args-of m))))
    (cond ((ge-equal arg1 arg2)
	   arg1)
	  ((ge-min-pair? arg1)
	   (simplify-min3 (first (args-of arg1))
			   (second (args-of arg1))
			   arg2))
	  ((ge-min-pair? arg2)
	   (simplify-min3 (first (args-of arg2))
			   (second (args-of arg2))
			   arg1))
	  ((falsifiable? (eqn> arg1 arg2) env)
           arg1)
	  ((falsifiable? (eqn> arg2 arg1) env)
	   arg2)
	  ((and (number? arg1)
		(number? arg2))
	   (min arg1 arg2))
	  (t m))))

;; innermost-index
;; The environment given to this function contains the constraints on
;; index variables imposed by integer looping constructs.  They are
;; assumed to be ordered innermost to outermost.

;; innermost-index returns the innermost index variable that appears in the
;; given expression.
(defun innermost-index (ge env)
  (loop for v in env do
        (if (depends-on? ge (car v))
	    (return (car v)))))

;; restrict takes an environment and an inequality and returns
;; a new environment which is identical to the one given with
;; the added restriction that the inequality is also satisfied.
;; It does this by constraining the interval of the innermost
;; loop index (that appears in the inequality) to meet the condition
;; of the inequality.
(defun restrict (env ineq)
  (let ((v (innermost-index ineq env))
	(newenv (copy-alist env)))
    (if (null v)
	(error "Cannot find index to restrict environment.")
	(progn
	  (rplacd (assoc v newenv :test #'eql)
		  (restrict-sequence (cdr (assoc v newenv :test #'eql))
				     v
				     ineq
				     env))
	  newenv))))

(defmethod restrict-sequence ((i integer-sequence)
			      (v ge-variable)
			      (ineq ge-equation)
			      env)
  (let ((ineqint (interval-from-ineq ineq v)))
    (if (null ineqint)
	i
	(interval-intersect i ineqint env))))

(defmethod restrict-sequence ((i integer-sequence)
			      (v ge-variable)
			      (ineq-list list)
			      env)
  (if (null ineq-list)
      i
      (restrict-sequence (restrict-sequence i v (car ineq-list) env)
			 v
			 (cdr ineq-list)
			 env)))

;; single-iteration? is a predicate which returns true if the the
;; given integer-sequence has only a single element in it.  That is,
;; if its start and end points are equal.
(defmethod single-iteration? (i)
  (declare (ignore i))
  nil)

(defmethod single-iteration? ((i integer-sequence))
  (ge-equal (weyli::start-of i) (weyli::end-of i)))

;; empty-interval? is a predicate which returns true if in the given
;; environment, it can be shown (using falsifiable?) that there are
;; no elements in the sequence.
(defmethod empty-interval? ((i integer-sequence) env)
  (cond ((and (number? (weyli::start-of i))
	      (number? (weyli::end-of i)))
	 (> (weyli::start-of i) (weyli::end-of i)))
	((ge-max-pair? (weyli::start-of i))
	 (let ((maxl (first (args-of (weyli::start-of i))))
	       (maxr (second (args-of (weyli::start-of i)))))
	   (and (empty-interval? (make-integer-sequence maxl (weyli::end-of i))
				 (restrict env (eqn>= maxl maxr)))
		(empty-interval? (make-integer-sequence maxr (weyli::end-of i))
				 (restrict env (eqn>= maxr maxl))))))
	((ge-min-pair? (weyli::end-of i))
	 (let ((minl (first (args-of (weyli::end-of i))))
	       (minr (second (args-of (weyli::end-of i)))))
	   (and (empty-interval? (make-integer-sequence (weyli::start-of i) minl)
				 (restrict env (eqn>= minr minl)))
		(empty-interval? (make-integer-sequence (weyli::start-of i) minr)
				 (restrict env (eqn>= minl minr))))))
	(t (falsifiable? (eqn>= (weyli::end-of i) (weyli::start-of i))
			 env))))

(defun lin-split-and-simplify (p env)
  (let ((localenv (cdr (assoc 'split-env env))))
  (lin-simplify (lin-split p :env env) localenv)))

(defvar *split-sparse*)
(defun sparsity-specialize (p env spec)
  (setf *split-sparse* spec)
  (lin-split-and-simplify p env))

(defun sparsity-specialize-list (p env spec-list)
  (loop for spec in spec-list
        do (setq p (sparsity-specialize p env spec)))
  p)

(defun env-add (x y env)
  (if (null (assoc x env))
      (nconc env `((,x . ,y)))
      (progn (rplacd (assoc x env) y)
	     env)))

(defmethod is-?-variable? (sym)
  (declare (ignore sym))
  nil)

(defmethod is-?-variable? ((sym ge-variable))
  (member #\? (user::coerce (string-of sym) 'list)))

(defmacro defsimp (name arglist &rest pairs)
  `(quote (,(parse-program `(,name ,@arglist))
	    ,@(loop for pair in pairs
		    collect (cons (if (consp (first (first pair)))
				      (mapcar #'parse-program (first pair))
				      (parse-program (first pair)))
			          (parse-program (second pair)))))))

(defmethod use-substitution (ge sub)
  (declare (ignore sub))
  ge)

(defmethod use-substitution ((ge ge-or-numeric) sub)
  (let ((ge-ret ge))
    (loop for (v . expr) in sub
	  do (setq ge-ret (substitute expr v ge-ret)))
    ge-ret))

(defmethod use-substitution ((ge-list list) sub)
  (mapcar #'(lambda (ge) (use-substitution ge sub)) ge-list))

(defmethod find-substitution (a b)
  (declare (ignore a) (ignore b))
  nil)
(defmethod find-substitution ((app1 ge-application) (app2 ge-application))
  (cond ((not (equal (name-of (funct-of app1)) (name-of (funct-of app2))))
	 nil)
	((not (eq (length (args-of app1)) (length (args-of app2))))
	 nil)
	(t
	  (let ((sub nil))
	    (loop for app1arg in (args-of app1)
	          for app2arg in (args-of app2)
		  when (is-?-variable? app1arg)
		    do (setq sub (cons (cons app1arg app2arg) sub))
		  else if (not (ge-same-var app1arg app2arg))
			 do (return-from find-substitution nil))
	    (values t sub)))))
	       
(defmethod split-term? (a b c)
  (declare (ignore a) (ignore b) (ignore c))
  nil)

(defmethod split-term? ((g ge-application) sspec env)
  (multiple-value-bind (applies? sub) (find-substitution (first sspec) g)
    (if applies?
      (let ((n 0))
        (loop for i in (cdr sspec) do
	  (if (not (falsifiable? (use-substitution (car i) sub) env))
	      (setq n (+ n 1))))
        (if (> n 1)
	    (list
	      (innermost-index (use-substitution (first (first (cdr sspec)))
						 sub)
			        env)
	      sub
	    nil)))
      nil)))

;; Use *bindings* to
(defun lin-split (program &key (env nil))
  (walk-form program #'linsplit-walker :env (copy-tree env)))

(defvar *linsplit-walkers* ())

(defun insert-linsplit-walker (context type func)
  (let ((assoc-list (assoc context *linsplit-walkers*)))
    (cond ((null assoc-list)
           (push (list context (list type func))
                 *linsplit-walkers*))
          (t (loop for pair in (rest assoc-list)
                   do (when (eql (first pair) type)
                        (setf (cl:second pair) func)
                        (return t))
                   finally (push (list type func) (cl:rest assoc-list)))))))

(defun find-linsplit-walker (form context)
  (let ((assoc-list (assoc context *linsplit-walkers*)))
    (when assoc-list
      (loop for (type func) in (rest assoc-list)
            do (when (typep form type) 
                 (return func))))))

(defun linsplit-walker (form context env)
  (let ((func (find-linsplit-walker form context)))
    (if func (funcall func form env)
        form)))

(defmacro define-linsplit-walker (type context args &body body)
  (let ((func-name (intern (format nil "LINSPLIT-~A-~A" context type))))
    `(progn (defun ,func-name ,args ,@body)
            (insert-linsplit-walker ,context ',type ',func-name)
	    ',func-name)))

(define-linsplit-walker loop-int :eval (form env)
  (let ((newenv (acons (index-of form)
		       (interval-of form)
		       (cdr (assoc 'split-env env)))))
  (let* ((vsub (split? form newenv))
	 (v (first vsub))
	 (sub (second vsub)))
    (cond ((and v (ge-same-var v (index-of form)))
	   (values
	    (make-sequence
	     (loop for i in (cdr *split-sparse*)
		 collect (make-loop-int v
			      (restrict-sequence (interval-of form)
						 v
						 (use-substitution (car i) sub)
						 (cdr (assoc 'split-env env)))
			      (body-of form))))
	    :done))
	  (t (env-add 'split-env newenv env)
	     form)))))

;; split? recursively searches a subtree to determine which loop
;; to split
(defmethod split? (form env)
  (declare (ignore form) (ignore env))
  nil)

(defmethod split? ((form scopes-term) env)
  (split? (term-of form) env))
   
(defmethod split? ((form ge-application) env)
  (let ((split-var (split-term? form *split-sparse* env)))
    (if split-var
	split-var
	(loop for arg in (args-of form) do
	  (let ((f (split? arg env)))
	    (if f (return f)))))))
      
(defmethod split? ((form assignment) env)
  (split? (value-of form) env))

(defmethod split? ((form sequence) env)
  (loop for term in (terms-of form) do
    (let ((f (split? term env)))
      (if f (return f)))))

(defmethod split? ((form integer-sequence) env)
  (or (split? (weyli::start-of form) env)
      (split? (weyli::end-of form) env)))
  
(defmethod split? ((form loop-int) env)
  (or (split? (body-of form)
	      (acons (index-of form)
		     (interval-of form)
		     env))
      (split? (interval-of form) env)))

(defmethod split? ((form ge-nary) env)
  (loop for arg in (terms-of form) do
    (let ((f (split? arg env)))
      (if f (return f)))))
	   
(defmethod split? ((form ge-expt) env)
  (let ((b (split? (base-of form) env)))
    (if b
	b
	(split? (exponent-of form) env))))


(defmethod lin-simplify (form env)
  (declare (ignore env))
  form)

(defmethod lin-simplify ((form program) env)
  (make-program (name-of form) (args-of form)
		(lin-simplify (term-of form) env)))

(defmethod lin-simplify ((form bind) env)
  (make-bind (bindings-of form)
	     (lin-simplify (term-of form) env)))

(defconstant *substitute-number* (coerce 'wrm::number *general*))

(defmethod is-*substitute-number* ((e ge-variable))
  (ge-same-var e *substitute-number*))

(defmethod is-*substitute-number* (e)
  (declare (ignore e))
  nil)

(defmethod lin-simplify ((form ge-application) env)
  (multiple-value-bind (applies? sub)
                       (find-substitution (first *split-sparse*) form)
    (if applies?
      (let ((s (loop for (i . v) in (cdr *split-sparse*)
		     do	(if (truthifiable? (use-substitution i sub) env)
			    (return (if (is-*substitute-number* v)
					form
					(use-substitution v sub)))))))
	(if s s form))
      form)))

(defmethod lin-simplify ((form assignment) env)
  (let ((v (lin-simplify (value-of form) env)))
    (if (ge-equal (location-of form) v)
	nil
	(make-assignment (location-of form) v))))

(defmethod lin-simplify ((form sequence) env)
  (let ((newterms '()))
    (loop for term in (terms-of form) do
	  (let ((term (lin-simplify term env)))
	    (if (not (null term))
		(setq newterms (append newterms (list term))))))
    (if (null newterms)
	nil
	(if (= 1 (length newterms))
	    (first newterms)
	    (make-sequence newterms)))))
  
(defmethod lin-simplify ((form loop-int) env)
  (let ((i (simplify-interval (lin-simplify (interval-of form) env) env)))
    (cond
      ((empty-interval? i env) nil)
      ((single-iteration? i)
       (lin-simplify (subst-spl (body-of form)
				(index-of form)
				(weyli::start-of i))
		     (acons (index-of form)
			    (interval-of form)
			    env)))
      (t
	(let ((b (lin-simplify (body-of form)
		              (acons (index-of form)
				     (interval-of form)
				     env))))
	   (if (null b)
	       nil
	       (make-loop-int (index-of form) i b)))))))

(defmethod lin-simplify ((form ge-nary) env)
  (let (new-terms changed?)
    (loop for arg in (terms-of form)
	  for new-form = (lin-simplify arg env)
	  do (push new-form new-terms)
	     (when (not (eq new-form arg))
	       (setq changed? t)))
    (when changed?
      (setq form (make-instance (class-of form)
				:domain (domain-of form)
				:terms (nreverse new-terms)))
      (possibly-simplify form))
    form))
	   
(defmethod lin-simplify ((form ge-expt) env)
  (let ((new-base (lin-simplify (base-of form) env))
	(new-expt (lin-simplify (exponent-of form) env)))
    (unless (and (eql new-base (base-of form))
		 (eql new-expt (exponent-of form)))
      (setq form 
	    (make-instance (class-of form)
			   :domain (domain-of form)
			   :base new-base :exp new-expt))
      (possibly-simplify form))
    form))

(defun print-ineq-system (stream ineq)
  (loop for i in ineq
	do (format stream ":~S~%" i)))

(defun make-ge-for-intervals (var exp)
  (cond ((ge-max-pair? exp)
           (append
	     (make-ge-for-intervals var (first (args-of exp)))
	     (make-ge-for-intervals var (second (args-of exp)))))
	((eq exp *negative-infinity*) nil)
	(t (list (canonicalize (eqn>= var exp))))))

(defun make-le-for-intervals (var exp)
  (cond ((ge-min-pair? exp)
           (append
	     (make-le-for-intervals var (first (args-of exp)))
	     (make-le-for-intervals var (second (args-of exp)))))
	((eq exp *positive-infinity*) nil)
	(t (list (canonicalize (eqn>= exp var))))))
	  
(defun make-ineq-for-intervals (var interval)
  (append (make-ge-for-intervals var (weyli::start-of interval))
	  (make-le-for-intervals var (weyli::end-of interval))))

(defun make-ineq-system (env)
  (loop for (var . interval) in env
	append (make-ineq-for-intervals var interval)))

;; falsifiable? returns true if the given inequality can be disproved
;; (proved false) given the constraints of the environment.
;; A return value of false does not necessarily indicate that the
;; given inequality is true; rather it indicates that falsifiable? was
;; unable to disprove it.
;;
;; Two version are given here.

;; The first version (commented out) is an exponential time case by case
;; decomposition of the problem.  It works in many simple cases but
;; fails to find many proofs when expressions contain max and min calls.

;; The second version utilizes a publicly available mixed linear integer
;; programming package (lpsolve), available through netlib, to disprove
;; the inequalities by constructing an infeasible integer programming
;; problem.  It is far more accurate than the first version.  Should the
;; lp solver change, it is likely that changed will be needed to code
;; here to correctly interpret the output of the lp program.

;(defmethod falsifiable? ((ineq ge-eqn>) env)
;  (let ((i (canonicalize ineq)))
;    (let ((res (testineq i)))
;      (cond ((null (testineq i))
;	     t)
;	    ((eq t res)
;	     nil)
;	    (t
;	     (let ((v (innermost-index i env)))
;	       (if (null v)
;		   nil
;		   (let ((newi (interval-from-ineq i v)))
;		     (if (null newi)
;			 nil
;		         (empty-interval?
;			   (interval-intersect
;			     (cdr (assoc v env :test #'ge-same-var))
;			     newi
;			     env)
;			   env))))))))))

;(defmethod falsifiable? ((ineq ge-eqn>=) env)
;  (let ((i (canonicalize ineq)))
;    (let ((res (testineq i)))
;      (cond ((null (testineq i))
;	     t)
;	    ((eq t res)
;	     nil)
;            (t
;	     (let ((v (innermost-index i env)))
;	       (if (null v)
;		   nil
;		   (let ((newi (interval-from-ineq i v)))
;		     (if (null newi)
;			 nil
;			 (empty-interval?
;			   (interval-intersect
;			     (cdr (assoc v env :test #'ge-same-var))
;			     newi
;			     env)
;			   env))))))))))

(defmethod eliminate-> ((ge ge-or-numeric))
  ge)

(defmethod eliminate-> ((ge ge-eqn>))
  (eqn>= (lhs-of ge) (coerce 1 *general*)))

(defmethod falsifiable? ((ineq ge-equation) env)
  (let ((i (canonicalize ineq)))
    (let ((res (testineq i)))
      (cond ((null res) t)
	    ((eq t res) nil)
            (t
	     (let ((v (innermost-index i env)))
	       (infeasible? v (cons (eliminate-> i) (make-ineq-system env))
			    (env-vars env))))))))

(defmethod falsifiable? ((ineq-list list) env)
  (let ((ilist (mapcar #'canonicalize ineq-list)))
    (let ((res (mapcar #'testineq ilist)))
      (cond ((some #'null res) t)
	    ((every #'(lambda (r) (eq t r)) res) nil)
            (t
	     (let ((v (innermost-index ilist env)))
	       (infeasible? v (append (mapcar #'eliminate-> ilist)
				      (make-ineq-system env))
			    (env-vars env))))))))

(defmethod truthifiable? ((ineq-list list) env)
  (loop for ineq in ineq-list
	always (falsifiable? (eqn-complement ineq) env)))

(defmethod truthifiable? ((ineq ge-equation) env)
  (falsifiable? (eqn-complement ineq) env))

