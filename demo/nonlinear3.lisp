;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

(defprogram NONLINEAR3 (a)
  (bind ((x nil (real-numbers))
	 (y nil (real-numbers))
	 (z nil (real-numbers)))
    (constrain (x y z)
	       ((eqn= (+ x y z) a)
		(eqn= (+ (* x x) (* y y) (* d z z)) b)
		(eqn= (+ (* x x x x) (* y y y y) (* z z z z))
		      c))
      (print y))))
