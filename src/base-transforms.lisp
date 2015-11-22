;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;                    General Tools for Transforms
;;; ===========================================================================
;;; (c) copyright 1993 Cornell University

;;; $id: upolynomial.lisp,v 2.11 1992/05/15 15:55:08 rz exp $

(in-package "WRMI")

;; This is the root class of all transforms.  It consists of a slot for 
;; the name of the transform, and the transform function itself.  More complex
;; types of transforms may include additional information.

(defclass spl-transform (has-name)
  ((transform :initarg :transform
              :accessor transform-of)))

(defmethod print-object ((obj spl-transform) stream)
  (format stream "#<SPL transform: ~A>"
	  (name-of obj)))

;; The menu-name-of method gives the name that should be placed in the menu
;; for this transform.  This may include additional information like the 
;; order of the method. 

(defmethod menu-name-of ((transform spl-transform))
  (name-of transform))

