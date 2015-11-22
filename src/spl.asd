;;; -*- Mode:Lisp; Package:CL-User; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;			    SPL System Definition
;;; ===========================================================================
;;; (c) Copyright 1989, 1993 Cornell University

;;; $Id: spl.system,v 1.7 1995/07/07 18:31:03 mardis Exp $

(in-package "ASDF")

(defsystem "spl"
  :depends-on (weyl)
  :components
   ((:file "package")
    (:file "support")
    (:file "weyl-extensions")
    (:file "walker" :depends-on ("support"))
    (:file "types" :depends-on ("support"))
    (:module pretty-printer
	     :pathname #p""
	     :depends-on ("walker")
	     :components (#+(or Lucid Allegro)
			  (:file "pretty-print")
			  #+(or CCL ACLPC)
			  (:file "npprint")))
    (:file "spaces" :depends-on ("support"))
    (:file "parser" :depends-on ("spaces" "walker"))
    (:file "base-transforms")
    (:file "subst" :depends-on ("parser" "base-transforms"))
    (:file "realify" :depends-on ("parser" "base-transforms"))
    (:file "alg-trans" 
           :depends-on ("parser" "weyl-extensions" "base-transforms"))
    (:module iv-tools 
	     :pathname #p""
	     :depends-on ("parser" "base-transforms")
	     :components ((:file "iv-tools1")
			  (:file "iv-tools2")))
    (:file "ode-formula" :depends-on ("iv-tools"))
    (:file "finite-element" :depends-on ("iv-tools"))
    (:module GUI-interface
	     :pathname #p""
	     :components
	      #+CCL((:file "mcl-interface"))
	      #+ACLPC ((:file "aclpc-interface")
		       (:file "ivp-dialog"))
	      #-(OR CCL ACLPC) nil
	     :depends-on ("ode-formula" "walker")
	     :components ((:file "MCL-interface")
			  (:file "MCL-patches")))))
