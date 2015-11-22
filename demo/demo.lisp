;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

(defprogram COMPLEX1 ((x (complex-numbers)) (y (complex-numbers)) (z (real-numbers)))
  (set! x (+ (* x y) (* y z))))


(defprogram complex2 ()
  (bind ((x nil (complex-numbers)))
    (constrain (x) ((eqn= (+ (* x x) x 1) 0))
      (print x))))

(defprogram LINEAR1 (a b c d e f)
  (bind ((x nil (real-numbers))
	 (y nil (real-numbers)))
    (constrain (x y)
	       ((eqn= (+ (* a x) (* b y)) e)
	        (eqn= (+ (* c x) (* d y)) f))
      (print (list x y)))))

(defprogram LINEAR2 (flag)
  (bind ((x nil (real-numbers))
	 (y nil (real-numbers))
	 (z nil (real-numbers)))
   (constrain (x y z)
	      ((eqn= (+  (* 2 x) (* 3 y) z)
		     2)
	       (eqn= (+ (* -1 x) (* 2 y) (* 5 z)) 1))
      (if (null flag)
	  (constrain (x y z)
		     ((eqn= (+ x y z) 2))
		     (print (list x y z)))
	  (constrain (x y z)
		     ((eqn= (+ x y z) 7))
		     (print (list x y z)))))))

(defprogram LINEAR3 (a b c d e f)
  (bind ((x nil (complex-numbers))
	 (y nil (complex-numbers)))
    (constrain (x y)
	       ((eqn= (+ (* a x) (* b y)) e)
	        (eqn= (+ (* c x) (* d y)) f))
      (print (list x y)))))

(defprogram NONLINEAR1 (a)
  (bind ((x nil (real-numbers))
	 (y nil (real-numbers)))
    (constrain (x y)
	       ((eqn= (+ (* x x) (* y y y))
		      a)
		(eqn= (+ (* x y) (* y y))
		      (- 1 a)))
      (print y))))

(defprogram NONLINEAR2 (a flag)
  (bind ((x nil (real-numbers))
         (y nil (real-numbers))
         (z nil (real-numbers)))
        (constrain (x y z)
		   ((eqn= (+ (* x x) (* y y) (* y z))
			  a)
		    (eqn= (+ x y z) 1))
	  (if (null flag)
            (constrain (x y z)
                       ((eqn= (+ (* x y) (* y y) (* x z))
                              (- 1 a)))
              (print (list x y z)))
            (constrain (x y z)
                       ((eqn= (+ (* x y) (* y z) (* x z))
                              (- 1 a)))
              (print (list x y z)))))))

(defprogram Prog1 (finish-time)
  (with-causal-time
    (bind-type ((function-space 
		  (differentiable-functions time-domain
					    (euclidean-space 1))))
      (bind ((x nil function-space))
	(constrain (x)
	    ((forall t time-domain 
		     (eqn= (+ (deriv (fn x t) t) (* (fn x t) (fn x t)))
			   (sin t)))
	     (eqn= (fn x 0) 0))
	  (loop ((:in t (interval (0.0 :closed) (finish-time :closed)
				  :step-size 0.1
				  :ambient time)))
		(print (fn x t))))))))

(defprogram Prog2 (finish-time)
  (with-causal-time
    (bind-type ((function-space 
		  (differentiable-functions time-domain
					    (euclidean-space 1))))
      (bind ((x nil function-space)
	     (y nil function-space))
	(constrain (x y)
	    ((forall t time-domain 
		     (eqn= (deriv (fn x t) t)
			   (+ (* 3 (fn x t)) (fn y t)))
		     (eqn= (deriv (fn y t) t)
			   (+  (fn x t) (* -2 (fn y t)))))
	     (eqn= (fn x 0) 1)
             (eqn= (fn y 0) 2))
	  (loop ((:in s (interval (0.0 :closed) (finish-time :closed)
				  :step-size 0.1
				  :ambient time)))
	        (print (+ (expt (fn x s) 2) (expt (fn y s) 2)))))))))

(defprogram Prog3 (finish-time)
  (with-causal-time
    (bind-type ((function-space 
		  (differentiable-functions time-domain
					    (euclidean-space 1))))
      (bind ((x nil function-space)
	     (y nil function-space))
	(constrain (x y)
	    ((forall t time-domain 
		     (eqn= (deriv (fn x t) t)
			   (+ (* 3 (fn x t)) (fn y t))))
	     (eqn= (fn x 0) 1)
             (eqn= (fn y 0) 2))
	  (loop ((:in t (interval (0.0 :closed) (finish-time :closed)
				  :step-size 0.1
				  :ambient time)))
		(if (> a 0)
		    (constrain (x y)
			       ((forall t time-domain
					(eqn= (deriv (fn y t) t)
					      (+  (fn x t) (* -2 (fn y t))))))
		       (print (+ (expt (fn x t) 2) (expt (fn y t) 2))))
		    (constrain (x y)
			       ((forall t time-domain
					(eqn= (deriv (fn y t) t)
					      (+  (fn x t) (* 2 (fn y t))))))
		       (print (+ (expt (fn x t) 2) (expt (fn y t) 2)))))))))))

(defprogram Square ((p (real-numbers)))
  (bind ((h nil (real-numbers))
         (w nil (real-numbers)))
    (constrain (h w) ((eqn= (+ h w) (/ p 2)))
      (maximize (h w) ((* h w))
         (print (list h w))))))

(defprogram Burgers1 (L finish-time)
  (with-causal-time
    (bind-type* ((space (euclidean-space 1))
		 (function-space (differentiable-functions
				   (direct-sum space time-domain)
				   (euclidean-space 1))))
      (bind ((u nil function-space))
        (constrain (u)
		   ((forall t time-domain
		      (forall x space
			(eqn= (+ (deriv (fn u x t) t)
				 (* (fn u x t) (deriv (fn u x t) x)))
			      (deriv (fn u x t) x x))
			(eqn= (fn u x t) (fn u (+ x L) t)))))
         ;; Still need initial conditions
	 (loop ((:in t (interval (0.0 :closed) (finish-time :closed)
				 :step-size 0.1
				 :ambient time)))
	   (loop ((:in x (interval (0.0 :closed) (10 :closed)
				 :step-size 0.1
				 :ambient space)))
	       (print (fn u x t)))))))))

(defprogram complex3 ()
  (with-causal-time 
    (bind ((z nil (differentiable-functions time-domain (complex-numbers))))
      (constrain (z) ((forall t time-domain 
			      (eqn= (deriv (fn z t) t)
				    (+ (fn z t) (* (fn z t) (fn z t)))))
		      (eqn= (fn z 0) 1))
	 (loop ((:in t (interval (0.0 :closed) (10.0 :closed)
				 :step-size 0.1
				 :ambient time)))
	       (print (fn z t)))))))
