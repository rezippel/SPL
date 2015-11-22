;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		 User interface for Apple Common Lisp
;;; ===========================================================================
;;; (c) Copyright 1997 Cornell University

(in-package "WRMI")

;; This is the basic window for working with SPL code
(defvar *spl-window* nil)
 
;; Return the window, creating it the first time or when it's closed.
;; Use only this function if you need only one instance.
(defun spl-window ()
   (if (cg:windowp *spl-window*) *spl-window* 
      (setq *spl-window* (make-spl-window))))
 
;; Create an instance of the window.
;; Use this if you need more than one instance.
(defun make-spl-window (&key (parent cg:*screen*) 
                        (window-interior (cg:make-box 524 201 1250 819)) 
                        (name :spl-window) (title "SPL-Window"))
   (setq cg:*loaded-but-uncreated-windows* 
      (delete 'spl-window cg:*loaded-but-uncreated-windows*))
   (let (window-0 window-1 window-2 window-3 window-4)
      (setq window-0 
         (cg:open-stream 'cg:text-edit-window 
            parent :io 
            :name name 
            :title title 
            :font (cg:make-font-ex :modern :courier 13 nil) 
            :window-state :shrunk 
            :window-border :frame 
            :left-attachment nil 
            :top-attachment nil 
            :right-attachment nil 
            :bottom-attachment nil 
            :user-movable t 
            :user-resizable t 
            :user-closable t 
            :user-shrinkable t 
            :user-scrollable t 
            :overlapped nil 
            :background-color nil 
            :pop-up-p nil 
            :window-interior window-interior))
      (setf (cg:window-editable-p window-0) t)
      (setf (getf (cg:stream-plist window-0) :path) 
         (let* ((pathname (load-time-value *load-pathname*)))
            (if 
               (or (not (pathnamep pathname)) 
                  (member (pathname-type pathname) acl:*fsl-extensions* 
                     :test #'string-equal))
               "C:\\rz\\Simlab\\spl\\src\\SPL-Window.bil" 
               (namestring pathname))))
      (setf (getf (cg:stream-plist window-0) :startup-state) nil)
      (setf (getf (cg:stream-plist window-0) :top-level-p) t)
      (setf (cg:help-string window-0) (delete #\Newline nil))
      (setf (getf (cg:stream-plist window-0) :package) nil)
      (cg:add-common-status-bar-to-window window-0 :font 
         (cg:make-font-ex :swiss :arial 13 nil) :parts nil :min-height 0)
      (setq window-1 
         (cg:open-dialog 
            (list 
               (cg:make-dialog-item 
                  :widget 'cg:button 
                  :name :button-1 
                  :title (delete #\Newline "Reload") 
                  :value 'spl-reload-file
                  :box (cg:make-box 2 2 95 24) 
                  :tabstop nil 
                  :groupstart nil 
                  :font (cg:make-font-ex nil :arial 13 '(:bold))))
            'cg:toolbar window-0 
            :name :toolbar-1 
            :title "Untitled.5" 
            :font (cg:make-font :swiss :system 16 '(:bold)) 
            :window-state :normal 
            :window-border :plain 
            :left-attachment :left 
            :top-attachment :top 
            :right-attachment :right 
            :bottom-attachment :top 
            :user-movable nil 
            :user-resizable nil 
            :user-closable nil 
            :user-shrinkable nil 
            :user-scrollable nil 
            :overlapped nil 
            :background-color cg:light-gray 
            :pop-up-p nil 
            :window-interior (cg:make-box 3 3 723 28)))
      (setf (cg:window-editable-p window-1) t)
      (setf (getf (cg:stream-plist window-1) :path) 
         (let* ((pathname (load-time-value *load-pathname*)))
            (if 
               (or (not (pathnamep pathname)) 
                  (member (pathname-type pathname) acl:*fsl-extensions* 
                     :test #'string-equal))
               "C:\\rz\\Simlab\\spl\\src\\SPL-Window.bil" 
               (namestring pathname))))
      (setf (getf (cg:stream-plist window-1) :startup-state) nil)
      (setf (getf (cg:stream-plist window-1) :top-level-p) nil)
      (setf (cg:help-string window-1) (delete #\Newline nil))
      (setf (getf (cg:stream-plist window-1) :package) nil)
      (cg:add-toolbar-to-window window-0 window-1)
      (setq window-1 (cg:frame-child window-0))
      (progn
         (cg:set-window-menu window-0 
            (let ((menu-bar (cg:open-stream 'cg:menu-bar cg:*screen* :io)))
               (cg:add-to-menu menu-bar 
                  (cg:make-menu-item 
                     :name :file-menu 
                     :title "~File" 
                     :value 
                     (cg:open-menu 
                        (list 
                           (cg:make-menu-item 
                              :name :new 
                              :title "~New" 
                              :value 'te:new-text-editor 
                              :event-synonym '(cg:control-key #\N) 
                              :plist '(pc::id 26369))
                           (cg:make-menu-item 
                              :name :open 
                              :title "~Open" 
                              :value 'open-spl-file 
                              :event-synonym '(cg:control-key #\O) 
                              :plist '(pc::id 26370))
                           (cg:make-menu-item 
                              :name :save 
                              :title "Save" 
                              :value 'te:save-text-file 
                              :event-synonym '(cg:control-key #\S) 
                              :plist '(pc::id 26371))
                           (cg:make-menu-item 
                              :name :save-as 
                              :title "Save ~As" 
                              :value 'te:save-as-text-file 
                              :plist '(pc::id 26372))
                           (cg:make-menu-item 
                              :title "-" 
                              :value "-" 
                              :available-p nil)
                           (cg:make-menu-item 
                              :name :exit 
                              :title "E~xit" 
                              :value 'cg:user-close-top-level-window 
                              :event-synonym '(cg:alt-key pc:vk-f4) 
                              :plist '(pc::id 26373)))
                        'cg:pop-up-menu cg:*screen* 
                        :name :default-menu 
                        :selection-function 
                        'cg:funcall-menu-item-with-window)))
               (cg:add-to-menu menu-bar 
                  (cg:make-menu-item 
                     :name :edit-menu 
                     :title "~Edit" 
                     :value 
                     (cg:open-menu 
                        (list 
                           (cg:make-menu-item 
                              :name :cut 
                              :title "~Cut" 
                              :value :cut 
                              :event-synonym '(cg:control-key #\X) 
                              :plist '(pc::id 26625))
                           (cg:make-menu-item 
                              :name :copy 
                              :title "C~opy" 
                              :value :copy 
                              :event-synonym '(cg:control-key #\C) 
                              :plist '(pc::id 26626))
                           (cg:make-menu-item 
                              :name :paste 
                              :title "~Paste" 
                              :value :paste 
                              :event-synonym '(cg:control-key #\V) 
                              :plist '(pc::id 26627)))
                        'cg:pop-up-menu cg:*screen* 
                        :name :default-menu 
                        :selection-function 
                        'funcall-menu-item-with-window)))
               (cg:add-to-menu menu-bar 
                  (cg:make-menu-item 
                     :name :transforms-menu 
                     :title "~Transforms" 
                     :value 
                     (cg:open-menu 
                        (list 
			 (cg:make-menu-item 
			  :name :cleanup-item 
			  :title "Cleanup Program" 
			  :value 'spl-cleanup-program
			  :plist '(pc::id 27137))
			 (cg:make-menu-item 
			  :title "Realify" 
			  :value 'spl-realify-buffer
			  :plist '(pc::id 27138))
			 (cg:make-menu-item 
			  :title "Initial Value Problems" 
			  :value 'spl-ivp-transform)
			 (cg:make-menu-item 
			  :title "Boundary Value Problems" 
			  :value "Boundary Value Problems" 
			  :plist '(pc::id 27139))
			 (cg:make-menu-item 
			  :title "Resolve" 
			  :value (cg:open-menu
				  (list
				   (cg:make-menu-item
				    :name :solve-linears
				    :title "(Solve) Linear Equations"
				    :value 'spl-solve-linear-equations
				    :plist '(pc::id 27140))
				   (cg:make-menu-item
				    :name :solve-linears
				    :title "(Triangularize) NonLinear Equations"
				    :value 'spl-triangularize-equations
				    :plist '(pc::id 27141))
				   (cg:make-menu-item
				    :name :solve-linears
				    :title "Maximizations"
				    :value 'spl-resolve-maximizations
				    :plist '(pc::id 27142))
				   (cg:make-menu-item
				    :name :solve-linears
				    :title "Minimizations"
				    :value 'spl-resolve-minizations
				    :plist '(pc::id 27143)))
				  'cg:pop-up-menu cg:*screen* 
				  :name :default-menu 
				  :selection-function 
				  'cg:funcall-menu-item-with-window)))
                        'cg:pop-up-menu cg:*screen* 
                        :name :default-menu 
                        :selection-function 'cg:funcall-menu-item-with-window)))
               menu-bar))
         (cg:reshape-window-interior window-0 window-interior))
      (let* ((box (getf cg:*window-exteriors* (cg:object-name window-0))))
         (when box (cg:reshape-window-exterior window-0 box)))
      (cg:show-window window-0 nil)
      window-0))

(unless (cg:windowp *spl-window*) 
   (pushnew 'spl-window cg:*loaded-but-uncreated-windows*))

(defvar *code-windows* (make-hash-table)
  "Table of code attached to each window")

(defun open-spl-file (window)
  (let ((window-state (gethash window *code-windows*))
        pathname fun)
     
    (setq pathname 
          (cg:ask-user-for-existing-pathname "Open SPL file"
	         :host (namestring (if window-state 
                                       (let ((path (first window-state)))
                                          (make-pathname :host (pathname-host path)
                                                         :directory (pathname-directory path)))
                                       *default-pathname-defaults*))
		 :allowed-types '(("Lisp files" . "*.lisp")
		 	          ("Lisp files" . "*.lsp"))))
     (when pathname
      (load pathname)
      (when (setq fun (parse-program *last-spl-function*))
	(set-window-program window pathname fun)))))
	
(defun spl-reload-file (window)
  (let ((pathname (gethash window *code-windows*))
	fun)
    (when pathname 
      (load pathname)
      (when (setq fun (parse-program *last-spl-function*))
	(set-window-program window pathname fun)))))

(defun spl-cleanup-program (window)
  (defun spl-realify-buffer (window) 
  (let* ((window-state (gethash window *code-windows*))
	 (prog (second window-state))
	new-prog)
    (setq new-prog (cleanup-program new-prog)))
  (set-window-program window (first window-state) new-prog)))

(defun spl-realify-buffer (window) 
  (let* ((window-state (gethash window *code-windows*))
	 (prog (second window-state))
	new-prog)
    (when prog
      (multiple-value-bind (vars cleanup?) (values nil t) ;; (realify-dialog fun)
	(declare (ignore vars))
	(setq new-prog (realify prog))
	(when cleanup?
	  (setq new-prog (cleanup-program new-prog)))
	(set-window-program window (first window-state) new-prog)))))

(defun set-window-program (window path func)
  (setf (gethash window *code-windows*) (list path func))
  (cg:device-clear-page window)
  (cg:with-delayed-redraw (window)
    (spl-print func window)))

;;;
;;; Initial value problems
;;;

(defvar *discretization-method* (first *IV-transforms*))
(defvar *ambient-space*)
(defvar *window-state*)

(defvar *steppers* (reverse *iv-steppers*))
(defvar *stepsizing-method*)

;; This is a dummy set of spaces to allow debugging of the dialogs.
(defvar *spaces* (list (make-binding (coerce 'time *general*)
				     (get-real-numbers)
				     nil)))

(defun spl-ivp-transform (window)
  (let* ((*window-state* (gethash window *code-windows*))
	 (*spaces* (ambient-spaces-of (second *window-state*)))
	 (*discretization-method* (first (last *IV-transforms*)))
	 (*stepsizing-method* 0)
	 (*ambient-space*)
	 (*steppers* (reverse *iv-steppers*))
	 (*stepsizing-method* (first *steppers*))
	 (*cleanup?* t)
         new-prog)
    (when *spaces*
      (setq *ambient-space* (value-of (first *spaces*)))
      (when (cg:pop-up-dialog (make-ivp-transforms :parent window))
	(when *ambient-space*
	  (setq new-prog
		(iv-advance (second *window-state*)
			    *ambient-space*
			    *discretization-method*
			    *stepsizing-method*))
	  (when *cleanup?*
	    (setq new-prog (cleanup-program new-prog)))
	  (set-window-program window (first *window-state*) new-prog))))))

(defun spl-solve-linear-equations (window)
  (let* ((*window-state* (gethash window *code-windows*))
	 (prog (second *window-state*))
	 new-prog)
    (setq new-prog (solve-linearconstraints prog))
    (setq new-prog (cleanup-program new-prog))
    (set-window-program window (first *window-state*) new-prog)))

(defun spl-triangularize-equations (window)
  (let* ((*window-state* (gethash window *code-windows*))
	 (prog (second *window-state*))
	 new-prog)
    (setq new-prog (solve-triangularize prog))
    (setq new-prog (cleanup-program new-prog))
    (set-window-program window (first *window-state*) new-prog)))

(defun spl-resolve-maximizations (window)
  (let* ((*window-state* (gethash window *code-windows*))
	 (prog (second *window-state*))
	 new-prog)
    (setq new-prog (resolve-maximize prog))
    (setq new-prog (cleanup-program new-prog))
    (set-window-program window (first *window-state*) new-prog)))

(defun spl-resolve-minimizations (window)
  (let* ((*window-state* (gethash window *code-windows*))
	 (prog (second *window-state*))
	 new-prog)
    (setq new-prog (resolve-minimize prog))
    (setq new-prog (cleanup-program new-prog))
    (set-window-program window (first *window-state*) new-prog)))

