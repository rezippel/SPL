;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  Geometric Classes
;;; ===========================================================================
;;; (c) Copyright 1992 Cornell University

;;; $Id: spaces.lisp,v 1.3 1995/07/27 17:06:16 mardis Exp $

(in-package "WRMI")

;; This is the root class of the different geometric objects
(defclass geometric-object (weyli::dimensional-space weyl::set) 
				
     ())

;; The following form makes it possible to compute the direct sum of
;; two geoemtric objects.  (I don't think there are any useful
;; operations on the direct sum yet.)

(weyli::define-direct-sum weyli::dimensional-space ())

(defmethod dimension-of ((domain direct-sum-dimensional-space))
  (length (weyli::tuple-value domain)))

(defclass has-name ()
     ((name :initform nil
	    :initarg :name
	    :accessor name-of)))

(defclass has-ambient ()
     ((ambient :initform nil :initarg :ambient :accessor
		     ambient-of)))

(defmethod ambient-space-of ((has-ambient has-ambient))
  (ambient-of has-ambient))

(defclass basic-interval (geometric-object has-name has-ambient)
     ((start :initarg :start :accessor start-of)
      (finish :initarg :finish :accessor finish-of)))

(defmethod initialize-instance :after ((obj basic-interval) &rest ignore)
  (declare (ignore ignore)))
  
(defmethod walk-math-expression ((form basic-interval) context env)
  (declare (ignore context env))
  form)

(defclass interval (basic-interval)
     ((start-type :initarg :start-type
		  :initform :closed
		  :accessor start-type-of)
      (finish-type :initarg :finish-type
		   :initform :close
		   :accessor finish-type-of)))


(defmethod print-object ((int interval) stream)
  (flet ((print-with-infs (x)
	   (cond ((eql x weyl:*positive-infinity*)
		  "inf")
		 ((eql x weyl:*negative-infinity*)
		  "-inf")
		 (t x))))
    (write-char
      (if (eql :closed (start-type-of int))
	  #\[ #\( )
      stream)
    (format stream "~A, ~A"
	    (print-with-infs (start-of int))
	    (print-with-infs (finish-of int)))
    (write-char
      (if (eql :closed (finish-type-of int))
	  #\] #\) )
      stream)))

(defclass periodic-interval (basic-interval)
     ())

(defmethod print-object ((int periodic-interval) stream)
  (format stream "Per:~s(~S, ~S)" (name-of int)(start-of int) (finish-of int)))

(defclass frequency-domain (geometric-object has-name has-ambient)
     ())

(defclass discrete-space ()
	())


;;; A UNIFORM-DISCRETE-INTERVAL represents a discretization of an INTERVAL.
;;; It reprsents the values {START, START+STEP-SIZE, START+2*STEP-SIZE, ...,
;;; FINISH}.

(defclass uniform-discrete-interval (interval discrete-space)
  ((step-size :initarg :step-size
	      :accessor step-size-of)))

(defmethod print-object ((int uniform-discrete-interval) stream)
  (format stream "~A~S..(~S)..~S~A"
	 (if (eql :closed (start-type-of int)) "[" "(") 
	 (start-of int) (step-size-of int) (finish-of int)
	 (if (eql :closed (finish-type-of int)) "]" ")")))

;;; A INTEGRAL-INTERVAL represents a mapping of a discretization of an
;;; INTERVAL onto a range of integers.

(defclass integral-interval (basic-interval discrete-space)
  ((step-size :initarg :step-size
	      :accessor step-size-of)))

(defmethod print-object ((int integral-interval) stream)
  (format stream "{~D,...,~D}" (start-of int) (finish-of int)))

(defmethod convert-to-continuous ((domain integral-interval) pt)
  (funcall (getf domain :continuous-map) pt))



