;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

(defprogram LINEAR1 (a b c d e f)
  (bind ((x nil (real-numbers))
	 (y nil (real-numbers)))
    (constrain (x y)
	       ((eqn= (+ (* a x) (* b y)) e)
	        (eqn= (+ (* c x) (* d y)) f))
      (print (list x y)))))