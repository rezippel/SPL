;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;			Pretty Printer
;;; ===========================================================================
;;; (c) copyright 1993 Cornell University

;;; $id: upolynomial.lisp,v 2.11 1992/05/15 15:55:08 rz exp $

(in-package "WRMI")

;; This file contains a pretty printer extensions for the code expressions
;; introduced in walker.lisp.  

;; Since SPL programs are not lists, but objects, we can use method
;; dispatch to decide which program fragment to use to pretty-print an
;; expression.  Thus we only need one form. 

(defvar *spl-indent* 0)

(defun spl-indent (stream)
  (fresh-line stream)
   (loop for i below *spl-indent*
         do (write-char #\Space stream)))

(defun spl-print (form &optional (stream *standard-output*))
  (spl-print% form stream))

(defmacro def-spl-print (type (form stream) &body body)
  `(progn
     (defmethod spl-print% ((,form ,type) ,stream)
       (spl-indent stream)
       (spl-princ ,form ,stream)
       (values))
     (defmethod spl-princ ((,form ,type) &optional (,stream *standard-output*))
       ,@body)))

(def-spl-print T (form stream)
  (write form :stream stream))

(def-spl-print list (form stream)
  (cond ((null form)
         (write-string "nil" stream))
        (t (write-char #\( stream)
           (spl-princ (first form) stream)
           (loop for term in (rest form)
                 do (write-char #\Space stream)
                 (spl-princ term stream))
           (write-char #\) stream))))

(defvar *color-highlight* nil)

;; Color doesn't work too well.  Stick with the bold for now.
(defmacro with-highlighting (stream &body forms)
  #+CCL
  `(if (typep ,stream 'ccl:fred-mixin)
     (let* ((buf (ccl:fred-buffer ,stream))           
            (old-color (ccl:get-fore-color stream))
            (old-style (ccl:buffer-current-font-spec buf)))
       (unwind-protect
         (progn 
           (cond (*color-highlight*
                  (ccl:fred-update stream)
                  (ccl:set-fore-color stream *color-highlight*))
                 (t 
                  (ccl::buffer-set-font-spec buf :bold)))
           ,@forms)
         (cond  (*color-highlight*
                 (ccl:fred-update stream)
                 (ccl:set-fore-color stream old-color)
                 (ccl:fred-update stream))
                (t
                 (ccl:buffer-set-font-spec buf old-style)))))
     (progn ,@forms))
  #+ACLPC
  `(progn ,@forms))

(def-spl-print weyli::domain-element (form stream)
  (with-highlighting stream
    (princ form stream)))

(def-spl-print weyli::domain (form stream)
  (with-highlighting stream
    (princ form stream)))

(def-spl-print application (form stream)
  (write-char #\( stream)
  (princ (funct-of form) stream)
  (when (args-of form)
    (write-char #\Space stream)
    (spl-princ (first (args-of form)) stream)
    (loop for arg in (rest (args-of form))
          do (write-char #\, stream) 
          (write-char #\Space stream)
          (spl-princ arg stream)))
  (write-char #\) stream))

(def-spl-print conditional (form stream)
  (write-char #\( stream)
  (write-string "IF" stream)
  (write-char #\Space stream)
  (spl-princ (predicate-of form) stream)
  (let ((*spl-indent* (+ *spl-indent* 4)))
    (spl-print (succeed-of form) stream)
    (when (not (null (fail-of form)))
      (spl-print (fail-of form) stream)))
  (write-char #\) stream))

(defvar *label-length* 8)

(def-spl-print sequence (form stream)
  (write-char #\( stream)
  (write-string "SEQUENCE" stream)
  (let* ((*spl-indent* (+ *spl-indent* 2))
         (term-indent *spl-indent*)
         (last-label? nil))
    (loop for term in (terms-of form)
          do (when (typep term 'label)
               (setq term-indent (+ *spl-indent* *label-length*))))
    (loop for term in (terms-of form) do 
       (cond ((typep term 'label)
              (spl-print term stream)
              (setq last-label? t))
             (t (let ((*spl-indent* term-indent))
                  (if last-label? 
                    (spl-princ term stream)
                    (spl-print term stream)))
                (setq last-label? nil))))
    (write-char #\) stream)))

(def-spl-print label (form stream)
  (princ (name-of form) stream)
  (write-char #\: stream)
  (let ((extra (- *label-length* (+ (length (string (name-of form))) 1))))
    (if (plusp extra)
      (loop for i below extra do (write-char #\Space stream))
      (write-char #\Space stream))))

(def-spl-print goto (form stream)
  (format stream "(GO ~A)" (target-of form)))

(def-spl-print assignment (form stream)
  (write-char #\( stream)
  (spl-princ (location-of form) stream)
  (write-string " <- " stream)
  (spl-princ (value-of form) stream)
  (write-char #\) stream))

(def-spl-print constrain (form stream)
  (write-char #\( stream)
  (write-string "CONSTRAIN" stream)
  (write-char #\Space stream)
  (write-char #\( stream)
  (spl-princ (first (vars-of form)) stream)
  (loop for v in (rest (vars-of form))
          do (write-char #\, stream) (write-char #\Space stream) 
          (spl-princ v stream))
  (write-char #\) stream)
  (spl-indent stream)
  (write-string "    (" stream)
  (let ((*spl-indent* (+ *spl-indent* 5)))
    (spl-princ (first (constraints-of form)) stream)
    (loop for c in (rest (constraints-of form))
          do (spl-print c stream))
    (write-char #\) stream))
  (let ((*spl-indent* (+ *spl-indent* 2)))
    (spl-print (term-of form) stream))
  (write-char #\) stream))

(def-spl-print extremize (form stream)
  (write-char #\( stream)
  (write-string (if (typep form 'minimize) "MINIMIZE" "MAXIMIZE")
		stream)
  (write-char #\Space stream)
  (write-char #\( stream)
  (spl-princ (first (vars-of form)) stream)
  (loop for v in (rest (vars-of form))
        do (write-char #\, stream) (write-char #\Space stream) 
           (spl-princ v stream))
  (write-char #\) stream)
  (spl-indent stream)
  (write-string "    (" stream)
  (let ((*spl-indent* (+ *spl-indent* 5)))
    (spl-princ (first (expressions-of form)) stream)
    (loop for c in (rest (expressions-of form))
          do (spl-print c stream))
    (write-char #\) stream))
  (let ((*spl-indent* (+ *spl-indent* 2)))
    (spl-print (term-of form) stream))
  (write-char #\) stream))

(def-spl-print binding (form stream)
  (write-char #\( stream)
  (spl-princ (var-of form) stream)
  (write-char #\Space stream)
  (spl-princ (value-of form) stream)
  (when (type-of form)
    (write-char #\Space stream)
    (spl-princ (type-of form) stream))
  (write-char #\) stream))

(def-spl-print bi-type (form stream)
  (format stream "[~A . " (structure-of form))
  (with-highlighting stream
    (princ (domain-of form) stream))
  (write-char #\] stream))

(def-spl-print bind (form stream)
  (write-char #\( stream)
  (write-string "BIND" stream)
  (write-char #\Space stream)
  (write-char #\( stream)
  (when (bindings-of form)
    (spl-princ (first (bindings-of form)) stream)
    (let ((*spl-indent* (+ *spl-indent* 7)))
      (loop for bind in (rest (bindings-of form))
            do (spl-print bind stream))))
  (write-char #\) stream)
  (let ((*spl-indent* (+ 2 *spl-indent*)))
    (spl-print (term-of form) stream))
  (write-char #\) stream))

(def-spl-print function (form stream)
  (write-char #\( stream)
  (write-string "LAMBDA" stream)
  (write-char #\Space stream)
  (princ (args-of form) stream)
  (let ((*spl-indent* (+ 2 *spl-indent*)))
    (spl-print (term-of form) stream))
  (write-char #\) stream))

(def-spl-print program (form stream)
  (write-char #\( stream)
  (write-string "DEFPROGRAM" stream)
  (write-char #\Space stream)
  (princ (name-of form) stream)
  (write-char #\Space stream)
  (spl-princ (args-of form) stream)
  (let ((*spl-indent* (+ 2 *spl-indent*)))
    (spl-print (term-of form) stream))
  (write-char #\) stream))

(def-spl-print universal-quantified-set (form stream)
  (write-char #\{ stream)
  (write-string "ForAll" stream)
  (write-char #\Space stream)
  (loop for (var set) in (bound-vars-of form)
	for first = t then nil
	do (unless first
	     (write-string ", " stream))
	   (spl-princ var stream)
	   (write-string " in " stream)
	   (spl-princ set stream))
  (write-char #\Space stream)
  (write-char #\. stream)
  (let ((*spl-indent* (+ 3 *spl-indent*)))
    (loop for exp in (exprs-of form)
          do (spl-print exp stream)))
  (write-char #\} stream))

