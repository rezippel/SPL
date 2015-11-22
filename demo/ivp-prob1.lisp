;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

(defprogram IVP-prob1 (finish-time)
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