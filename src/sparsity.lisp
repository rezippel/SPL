;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;	                  Sparsity specifications for the
;;;                           linspec transformation
;;; ===========================================================================
;;; (c) copyright 1995 Cornell University
(in-package 'wrm)

;; This file contains macros to create sparsity specifications used
;; by the linspec transformation to specialize matrix computations for
;; various types of sparsity.  
(user::import 'user::import)
(import 'user::defmacro)
(import 'user::defun)
(import 'wrmi::defsimp)

(defmacro wrmi::lower-tri (vv n)
  (user::let ((v (user::intern (user::symbol-name vv) 'wrm)))
  `(defsimp aref2 (,v i1? i2?)
     ((eqn> (+ i2? ,n) i1?) 0)
     ((eqn>= i1? (+ i2? ,n)) number))))
   
(defmacro wrmi::upper-tri (vv n)
  (user::let ((v (user::intern (user::symbol-name vv) 'wrm)))
  `(defsimp aref2 (,v i1? i2?)
     ((eqn> (+ i1? ,n) i2?) 0)
     ((eqn>= i2? (+ i1? ,n)) number))))
   
(defmacro wrmi::banded (vv u l)
  (user::let ((v (user::intern (user::symbol-name vv) 'wrm)))
  `(defsimp aref2 (,v i1? i2?)
     ((eqn> i2? (+ i1? ,u)) 0)
     (((eqn>= (+ i1? ,u) i2?)
       (eqn>= (+ i2? ,l) i1?)) number)
     ((eqn> i1? (+ i2? ,l)) 0))))

(defmacro wrmi::upper-triangular (v)
  `(wrmi::upper-tri ,v 0))

(defmacro wrmi::lower-triangular (v)
  `(wrmi::lower-tri ,v 0))

(defmacro wrmi::tri-diagonal (v)
  `(wrmi::banded ,v 1 1))

(defmacro wrmi::min-simp ()
  `(defsimp min-pair (a? b?)
    ((eqn> a? b?) b?)
    ((eqn>= b? a?) a?)))

(defmacro wrmi::max-simp ()
  `(defsimp max-pair (a? b?)
    ((eqn> a? b?) a?)
    ((eqn>= b? a?) b?)))

