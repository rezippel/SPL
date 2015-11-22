;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

(defprogram COMPLEX1 ((x (complex-numbers)) (y (complex-numbers)) (z (real-numbers)))
  (set! x (+ (* x y) (* y z))))


(defprogram complex2 ()
  (bind ((x nil (complex-numbers)))
    (constrain (x) ((eqn= (+ (* x x) x 1) 0))
      (print x))))