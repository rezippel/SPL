;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			       Support
;;; ===========================================================================
;;; (c) Copyright 1992 Cornell University

;;; $Id: support.lisp,v 1.1 1994/01/24 22:11:57 rz Exp $

(in-package "WRMI")

(defmethod first ((x list))
  (cl:first x))

(defmethod second ((x list))
  (cl:second x))

(defmethod third ((x list))
  (cl:third x))

(defmethod nth ((n integer) (l list))
  (cl:nth n l))

(defmethod nthcdr ((n integer) (l list))
  (cl:nthcdr n l))

(defmethod rest ((x list))
  (cl:rest x))

(defmacro def-walker-form (type)
  (let ((definer (intern (format nil "DEF-PROGRAM-~A" type))))
    `(defmacro ,definer (name args &body body)
       (let ((func-name (if (atom args) args
			    (intern (format nil "~A-~A" 
                                            name ,(string-capitalize 
                                                   (string type)))))))
	 `(progn
	    ,@(unless (atom args)
		      `((defun ,func-name ,args ,@body)))
	    (setf (get ',(intern (symbol-name name) (find-package 'WRM))
		       ',(intern (format nil "PROGRAM-~A" ',type)))
		  ',func-name))))))
