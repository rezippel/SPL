;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

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