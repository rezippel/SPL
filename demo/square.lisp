(in-package :wrm)

(defprogram Square ((p (real-numbers)))
  (bind ((h nil (real-numbers))
         (w nil (real-numbers)))
    (constrain (h w) ((eqn= (+ h w) (/ p 2)))
      (maximize (h w) ((* h w))
         (print (list h w))))))