;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

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
