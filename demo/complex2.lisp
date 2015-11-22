;;; -*- Mode:Lisp; Package: WRM; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

(in-package "WRM")

(defprogram complex2 ()
  (bind ((x nil (complex-numbers)))
    (constrain (x) ((eqn= (+ (* x x) x 1) 0))
      (print x))))
