;;; -*- Mode:Lisp; Package:CL-User; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			    WRM Package Definition
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; $Id: package.lisp,v 1.1 1994/01/24 22:11:53 rz Exp $

(in-package "CL-USER")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'WRMI)
    (make-package "WRMI" :use '(cl))
    (shadow '(function variable sequence make-sequence
	      first second third nth nthcdr rest)
	    'WRMI)
    #+Lucid
    (import '(lcl:copy-pprint-dispatch lcl:*print-pprint-dispatch* 
	      lcl:set-pprint-dispatch lcl:pprint-logical-block
	      lcl:pprint-exit-if-list-exhausted lcl:pprint-pop
	      lcl:pprint-indent lcl:pprint-newline) 'wrmi)
    (use-weyl-package (find-package 'WRMI))))

;; Code that is actually to be transformed is in the WRM package
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'WRM)
    (make-package "WRM" :use '())
    #+ignore
    (import '(weyl:+ weyl:- weyl:* weyl:/ weyl:expt weyl:deriv weyl:eqn=)
	    'WRM)))
