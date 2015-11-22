;;; -*- mode:lisp; package: WRMI; base:10; lowercase:t; syntax:common-lisp -*-
;;; ===========================================================================
;;;	     Initial Value Problem Dialog
;;; ===========================================================================
;;; (c) copyright 1997 Cornell University

;;; $id: ivp-dialog.lisp,v 2.11 1992/05/15 15:55:08 rz exp $
 
(in-package :wrmi)
 
(defvar *ivp-transforms* nil)
 
;; Return the window, creating it the first time or when it's closed.
;; Use only this function if you need only one instance.
(defun ivp-transforms ()
   (if (cg:windowp *ivp-transforms*) *ivp-transforms* 
      (setq *ivp-transforms* (make-ivp-transforms))))
 
;; Create an instance of the window.
;; Use this if you need more than one instance.
(defun make-ivp-transforms (&key (parent cg:*lisp-frame-window*) 
                             (window-interior 
                                (cg:make-box 398 156 908 449))
                             (name :ivp-transforms) 
                             (title 
                                "Initial Value Problem Transforms"))
   (setq cg:*loaded-but-uncreated-windows* 
      (cl:delete 'ivp-transforms cg:*loaded-but-uncreated-windows*))
   (let (cg:window-0 cg:window-1 cg:window-2 cg:window-3 cg:window-4)
      (setq cg:window-0 
         (cg:open-dialog 
            (list 
               (cg:make-dialog-item 
                  :widget 'cg:static-text 
                  :name :ambient-label 
                  :title (cl:delete #\Newline "Static Text 5") 
                  :value 
                  (cl:delete #\Newline "Space to be discretized") 
                  :box (cg:make-box 33 58 236 80) 
                  :tabstop nil 
                  :groupstart t 
                  :justification :right 
                  :font 
                  (cg:make-font-ex :roman :bookman\ old\ style 16 nil))
               (cg:make-dialog-item 
                  :widget 'cg:combo-box 
                  :name :combo-box-1 
                  :title (cl:delete #\Newline "Discretization Space Combo Box") 
                  :value 0
                  :box (cg:make-box 253 63 473 197) 
                  :tabstop t 
                  :groupstart t 
                  :range (loop for i below (length *spaces*)
			       collect i)
                  :key (let ((spaces *spaces*))
			 #'(lambda (i) 
			     (format nil "Domain: ~S" (var-of (nth i spaces)))))
                  :font (cg:make-font-ex nil :arial 13 nil)
		  :set-value-fn 
		  #'(lambda (widget new old)
		      (cond ((numberp new)
			     (setq *ambient-space* 
				   (value-of (nth new *spaces*)))
			     t))))
               (cg:make-dialog-item 
                  :widget 'cg:static-text 
                  :name :static-text-3 
                  :title (cl:delete #\Newline "Static Text 3") 
                  :value (cl:delete #\Newline "Method") 
                  :box (cg:make-box 95 136 236 157) 
                  :tabstop nil 
                  :groupstart t 
                  :justification :right 
                  :font 
                  (cg:make-font-ex :roman :bookman\ old\ style 16 nil))
               (cg:make-dialog-item 
                  :widget 'cg:combo-box 
                  :name :combo-box-3 
                  :title (cl:delete #\Newline "Steppers Combo Box") 
                  :value 0
                  :value-equality-test #'eql
                  :box (cg:make-box 253 135 473 225) 
                  :tabstop t 
                  :groupstart t 
                  :range (append (loop for i below (length *steppers*)
				       collect i)
				 (list :other))
                  :key (let ((steppers *steppers*))
			 #'(lambda (i)
			     (if (eql i :other) "Other step size"
				 (pretty-name-of (nth i steppers)))))
                  :font (cg:make-font-ex nil :arial 13 nil)
		  :set-value-fn
		  #'(lambda (widget new old)
		      (cond ((numberp new)
			     (setq *stepsizing-method* (nth new *steppers*))
			     t))))
               (cg:make-dialog-item 
                  :widget 'cg:editable-text 
                  :name :editable-text-1 
                  :title (cl:delete #\Newline "Editable Text 1") 
                  :value (cl:delete #\Newline "0.1") 
                  :box (cg:make-box 249 163 347 187) 
                  :tabstop t 
                  :groupstart t 
                  :font (cg:make-font-ex nil :courier 13 nil))
               (cg:make-dialog-item 
                  :widget 'cg:static-text 
                  :name :static-text-4 
                  :title (cl:delete #\Newline "Static Text 4") 
                  :value (cl:delete #\Newline "step size") 
                  :box (cg:make-box 360 163 465 187) 
                  :tabstop nil 
                  :groupstart t 
                  :font 
                  (cg:make-font-ex :roman :bookman\ old\ style 16 nil))
               (cg:make-dialog-item 
                  :widget 'cg:combo-box 
                  :name :numerical-method 
                  :title (cl:delete #\Newline "Integration Method Combo Box") 
                  :value (menu-name-of *discretization-method*)
		  :value-equality-test 'equal
                  :box (cg:make-box 253 29 473 213) 
                  :tabstop t 
                  :groupstart t 
                  :range (loop for iv-transform in (reverse *IV-transforms*)
			       collect (menu-name-of iv-transform)) 
                  :key 'cg:capitalize-object 
                  :font (cg:make-font-ex nil :arial 13 nil)
		  :set-value-fn
		  #'(lambda (widget new old)
		      (loop for iv-transform in (reverse *IV-transforms*)
			    do (when (equal new (menu-name-of iv-transform))
				 (setq *discretization-method* iv-transform)
				 (return t)))))
               (cg:make-dialog-item 
                  :widget 'cg:static-text 
                  :name :method-label 
                  :title (cl:delete #\Newline "Static Text 4") 
                  :value (cl:delete #\Newline "Integration method") 
                  :box (cg:make-box 33 29 236 49) 
                  :tabstop nil 
                  :groupstart t 
                  :justification :right 
                  :font 
                  (cg:make-font-ex :roman :bookman\ old\ style 16 nil))
               (cg:make-dialog-item 
                  :widget 'cg:check-box 
                  :name :cleanup 
                  :title 
                  (cl:delete #\Newline "Cleanup code when finished?") 
                  :value 'true 
                  :box (cg:make-box 21 261 303 282) 
                  :tabstop nil 
                  :groupstart nil 
                  :font (cg:make-font-ex nil :arial 13 '(:bold)))
               (cg:make-dialog-item 
                  :widget 'cg:default-button 
                  :name :transform? 
                  :title (cl:delete #\Newline "Transform") 
                  :box (cg:make-box 380 214 486 245) 
                  :tabstop nil 
                  :groupstart nil 
                  :set-value-fn 'cg:return-t-from-pop-up-dialog 
                  :font (cg:make-font-ex nil :arial 13 '(:bold)))
               (cg:make-dialog-item 
                  :widget 'cg:cancel-button 
                  :name :cancel? 
                  :title (cl:delete #\Newline "Cancel") 
                  :value t 
                  :box (cg:make-box 380 253 486 282) 
                  :tabstop nil 
                  :groupstart nil 
                  :set-value-fn 'cg:return-nil-from-pop-up-dialog 
                  :font (cg:make-font-ex nil :arial 13 '(:bold)))
               (cg:make-dialog-item 
                  :widget 'cg:lisp-group-box 
                  :name :lisp-group-box-1 
                  :title (cl:delete #\Newline "Step sizing technique") 
                  :box (cg:make-box 21 119 486 195) 
                  :tabstop nil 
                  :groupstart nil 
                  :font (cg:make-font-ex nil :arial 13 '(:bold)))
               (cg:make-dialog-item 
                  :widget 'cg:lisp-group-box 
                  :name :lisp-group-box-2 
                  :title (cl:delete #\Newline "Integration technique") 
                  :box (cg:make-box 21 9 486 100) 
                  :tabstop nil 
                  :groupstart nil 
                  :font (cg:make-font-ex nil :arial 13 '(:bold))))
            'cg:dialog parent 
            :name name 
            :title title 
            :font (cg:make-font :swiss :system 16 '(:bold)) 
            :window-state :shrunk 
            :window-border :frame 
            :left-attachment nil 
            :top-attachment nil 
            :right-attachment nil 
            :bottom-attachment nil 
            :user-movable t 
            :user-resizable nil 
            :user-closable t 
            :user-shrinkable nil 
            :user-scrollable nil 
            :overlapped nil 
            :background-color cg:light-gray 
            :pop-up-p t 
            :window-interior window-interior))
      (setf (cg:window-editable-p cg:window-0) t)
      (setf (cl:getf (cg:stream-plist cg:window-0) :path) 
         (let* ((pathname (load-time-value *load-pathname*)))
            (if 
               (or (not (pathnamep pathname)) 
                  (cl:member (pathname-type pathname) 
                     acl:*fsl-extensions* :test #'string-equal))
               "C:\\rz\\Simlab\\spl\\src\\IVP-Dialog.bil" 
               (namestring pathname))))
      (setf (cl:getf (cg:stream-plist cg:window-0) :startup-state) 
         :pop-up)
      (setf (cl:getf (cg:stream-plist cg:window-0) :top-level-p) nil)
      (setf (cg:help-string cg:window-0) (cl:delete #\Newline nil))
      (setf (cl:getf (cg:stream-plist cg:window-0) :package) :wrmi)
      nil
      (let* ((cg:box 
                (cl:getf cg:*window-exteriors* 
                   (cg:object-name cg:window-0))))
         (when cg:box (cg:reshape-window-exterior cg:window-0 cg:box)))
      (cg:show-window cg:window-0 :shrunk)
      cg:window-0))
(unless (cg:windowp *ivp-transforms*) 
   (pushnew 'ivp-transforms cg:*loaded-but-uncreated-windows*))
