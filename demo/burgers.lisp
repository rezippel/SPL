;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

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