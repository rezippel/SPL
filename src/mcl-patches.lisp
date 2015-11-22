;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		 MCL Patches for WRM
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University

(in-package "CCL")

(setq *warn-if-redefine* nil)
(setq *warn-if-redefine-kernel* nil)

(defun clear-edit-history (w)
  (let ((hist (fred-history w)))
    (when hist
      (let ((hh hist))
        (loop 
          (rplaca hh nil)
          (setq hh (cdr hh))
          (when (eq hh hist)
            (return)))))
    (wrmi::clear-spl-history w)))

(setq *warn-if-redefine* t)
(setq *warn-if-redefine-kernel* t)
