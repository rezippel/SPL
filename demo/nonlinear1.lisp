;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

(defprogram NONLINEAR1 (a)
  (bind ((x nil (real-numbers))
	 (y nil (real-numbers)))
    (constrain (x y)
	       ((eqn= (+ (* x x) (* y y y))
		      a)
		(eqn= (+ (* x y) (* y y))
		      (- 1 a)))
      (print y))))
