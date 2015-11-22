;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			  SPL's Type System
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University

(in-package "WRMI")

;; Full blown types (bi-types) always have STRUCTURE-OF and DOMAIN-OF
;; methods.

(defclass structure-type ()
     ())

(defmethod structure-type? ((x t)) nil)
(defmethod structure-type? ((x structure-type)) t)

(defmethod domain-type? ((x t)) nil)
(defmethod domain-type? ((x weyli::domain)) t)

(defvar *type-hash-table* (make-hash-table :test #'equal))

(defun reset-type-hash-table ()
  (setq *type-hash-table* (make-hash-table :test #'equal)))

(defmacro memoize-type (expression &body body)
  `(let ((.expr. ,expression))
     (multiple-value-bind (value found?) (gethash .expr. *type-hash-table*)
       (unless found?
	 (setq value (progn ,@body))
	 (setf (gethash .expr. *type-hash-table*) value))
       value)))

;; The following two are structure types, 

(defclass fixnum-type (structure-type)
     ((precision :initarg :precision
		 :accessor precision-of)))

(defmethod print-object ((obj fixnum-type) stream)
  (format stream "Fixnum(~D)" (precision-of obj)))

(defun wrm::fixnum (&optional (prec 32))
  (memoize-type `(wrm::fixnum ,prec)
    (make-instance 'fixnum-type :precision prec)))

(defclass flonum-type (structure-type)
     ((precision :initarg :precision
		 :accessor precision-of)))

(defmethod print-object ((obj flonum-type) stream)
  (format stream "Flonum(~D)" (precision-of obj)))

(defun wrm::flonum (&optional (prec 64))
  (weyli::memoize `(wrm::flonum ,prec)
    (make-instance 'flonum-type :precision prec)))

(defclass bi-type ()
     ((structure :initarg :structure
		 :accessor structure-of)
      (domain :initarg :domain
	      :accessor domain-of)))

(defun ensure-bi-type (type)
  (when (domain-type? type)
    (setq type (wrm::type nil type)))
  (when (structure-type? type)
    (setq type (wrm::type type nil)))
  type)

;; Here the primitive Domains

(defun wrm::rational-integers () (get-rational-integers))
(defun wrm::rational-numbers () (get-rational-numbers))
(defun wrm::real-numbers () (get-real-numbers))
(defun wrm::complex-numbers () (get-complex-numbers))
(defun wrm::euclidean-space (dim) (weyli::make-euclidean-space dim))

;; Domain type constructors
(defun wrm::polynomial-ring (dom vars) (get-polynomial-ring dom vars))
(defun wrm::ring-of-fractions (dom) (get-quotient-field dom))

(defun wrm::differentiable-functions (funct-domain funct-range &optional order)
  (declare (ignore order))
  (make-instance 'function-space :domain funct-domain :range funct-range))

(defmethod bi-type? ((x t)) nil)
(defmethod bi-type? ((x bi-type)) t)

(defmethod print-object ((obj bi-type) stream)
  (format stream "[~A . ~A]" (structure-of obj) (domain-of obj)))

(defun wrm::type (structure domain)
  (memoize-type `(wrm::type ,structure ,domain)
    (unless (or (null structure) (structure-type? structure))
      (describe structure)
      (error "~S was expected to be a structure type" structure))
    (unless (or (null domain) (domain-type? domain))
      (error "~S was expected to be a domain type" domain))
    (unless (or (null structure) (null domain))
      (error "One of structure and domain must be non-NIL"))
    (make-instance 'bi-type :structure structure
		   :domain domain)))

(defclass vector-type ()
     ((elt-type :initarg :elt-type
		:accessor elt-type-of)
      (length :initarg :length
	      :accessor length-of)))

(defmethod structure-type? ((vect vector-type))
  (structure-type? (elt-type-of vect)))

(defmethod domain-type? ((vect vector-type))
  (domain-type? (elt-type-of vect)))

(defmethod print-object ((obj vector-type) stream)
  (format stream "(Vector[~A] ~A)"
	  (length-of obj) (elt-type-of obj)))

(defun wrm::vector (length type)
  (memoize-type `(wrm::vector ,length ,type)
    (make-instance 'vector-type :length length :elt-type type)))

(defmethod domain-of ((obj vector-type))
  nil)
     
