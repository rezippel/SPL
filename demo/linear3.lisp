;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

(defprogram LINEAR3 (a b c d e f)
  (bind ((x nil (complex-numbers))
	 (y nil (complex-numbers)))
    (constrain (x y)
	       ((eqn= (+ (* a x) (* b y)) e)
	        (eqn= (+ (* c x) (* d y)) f))
      (print (list x y)))))
