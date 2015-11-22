;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;			      Substitution Transformation
;;; ===========================================================================
;;; (c) copyright 1995 Cornell University

(in-package "WRMI")

;; subst-spl will take an SPL program or program fragment and
;; substitute in a form for a program variable.
;;
;; This substitution is applied globally without any regard
;; to program binding structure.  For example, if the given
;; program has a binding construct, the variable in the
;; binding construct will be replaced by the form yielding
;; an (almost certainly) incorrect program.

;; support functions for retrieving relevant data from
;; the environment
(defun subst-var (env)
  (cdr (assoc 'subst-var env)))

(defun subst-val (env)
  (cdr (assoc 'subst-val env)))
  
;; the walker functions
(defmethod subst-walker (form context env) form)

(defmethod subst-walker ((form weyli::ge-variable) context env)
  (if (weyli::ge-same-var form (subst-var env))
     (subst-val env)
     form))

;; SUBST-SPL - call this to substitute "newform" for
;; "var" in "fragment"
(defun subst-spl (fragment var newform)
  (walk-form fragment #'subst-walker :env (list (cons 'subst-var var)
					    (cons 'subst-val newform))))

