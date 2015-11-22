;;; -*- Mode:Lisp; Package:WRMI; Base:10; Lowercase:T; Syntax:Common-Lisp -*-
;;; ===========================================================================
;;;		 User interface for Apple Common Lisp
;;; ===========================================================================
;;; (c) Copyright 1993 Cornell University

(in-package "WRMI")

(defclass editor-region ()
  ((window :initarg :window             ; The window containing the region
           :accessor window-of)
   (start :initarg :start               ; Begining of region
          :accessor start-of)
   (end :initarg :end                   ; End of region
        :accessor end-of)))

;; At some point this should become a much more complex object.  In 
;; particular, it needs to have refernees for the beginning and ending of
;; each sub-expression of the program. Otherwise our operations will only
;; be applicable for entire, top-level programs. 
(defclass ed-program (editor-region)
  ((program :initarg :program
            :accessor program-of)))

(defvar *program-cache* ())
(defvar *transform-history* ())

(defun clear-spl-history (window)
  (let ((cache nil))
    (loop for region in *program-cache*
          do (unless (eql (window-of region) window)
               (push region cache)))
    (setq *program-cache* cache)))

;; Insert the program in the editor window W, pretty printed, and 
;; return an ED-PROGRAM object.
(defmethod insert-in-buffer ((w ccl:window) (p program))
  (let* ((buffer (ccl:fred-buffer w))
         (start (ccl:buffer-position buffer)) 
         (*include-variable-counter* nil))
    (spl-print p w)
    (push (make-instance 'ed-program :window w
                         :start start :end (ccl:buffer-position buffer)
                         :program p)
          *program-cache*)))

(defmethod find-program ((w ccl:window) pos)
  (flet ((find-prog ()
           (loop for ed-prog in *program-cache*
                 do (when (and (eql w (window-of ed-prog))
                               (<= (start-of ed-prog) pos (end-of ed-prog)))
                      (return ed-prog)))))
    (or (find-prog)
        (progn 
          (parse-program-in-window w)
          (find-prog))
        (error "Can't find program!"))))

(defmethod delete-program ((ed-prog ed-program))
  (unless (member ed-prog *program-cache*)
    (error "Program ~A doesn't exist anymore" ed-prog))
  (setq *program-cache* (delete ed-prog *program-cache*))
  (let* ((w (window-of ed-prog))
         (buffer (ccl:fred-buffer w)))
    (ccl:set-mark buffer (start-of ed-prog))
    (ccl:buffer-delete buffer (start-of ed-prog) (end-of ed-prog))))

(defun buffer-top-level-sexp-bounds (buffer)
  "Return the top-level sexp bounds, or nil if there is none.
The top level sexp starts with left paren in the first column.
The current position may be just before the left paren, 
or before the next top-level sexp."
  (let* ((sexp-start-string #.(format nil "~%("))
         (top-level-sexp-start
          (if (and (= (ccl:buffer-column buffer) 0)
                     (char-equal (ccl:buffer-char buffer) #\()) ;; looking at \(            (buffer-position buffer)
            (ccl:buffer-position buffer)
            (let ((foo (ccl:buffer-string-pos buffer sexp-start-string 
                                              :from-end t)))
              (and foo (+ foo 1))))))
    (if (null top-level-sexp-start)
      nil
      (multiple-value-bind (sexp-start sexp-end)
           (ccl:buffer-current-sexp-bounds buffer top-level-sexp-start)
        (if (null sexp-start) nil
          (values sexp-start sexp-end))
        ))))

(defun buffer-top-level-sexp (buffer)
  "Return the top-level sexp or nil if none."
  (let ((start (buffer-top-level-sexp-bounds buffer)))
    (if start
      (ccl:buffer-current-sexp buffer start)
      nil)))

(defmethod ed-current-UPCASE ((w ccl:fred-window))
  (let* ((buffer (ccl:fred-buffer w))
         (pos (ccl:buffer-position buffer))
         (sexp (ccl:buffer-current-sexp buffer pos)))
    (pprint sexp)
    nil))

(defun parse-program-in-window (w)
  (let* ((buffer (ccl:fred-buffer w))) 
    (multiple-value-bind (sexp-start sexp-end)
                         (buffer-top-level-sexp-bounds buffer)
      (declare (ignore sexp-end))
      (let ((sexp (let ((*package* (ccl::window-package w)))
                    (ccl:buffer-current-sexp buffer sexp-start)))
            prog)
        (ccl:set-mark buffer sexp-start)
        (ccl:ed-kill-forward-sexp w)
        (setq prog (parse-program (eval sexp)))
        (insert-in-buffer w prog)
        (ccl:fred-update w)
        prog))))

(defun ed-parse-spl-program ()
  (parse-program-in-window (ccl::front-window)))

(defun ed-realify-spl-program ()
  (let* ((w (ccl:front-window))
         (buffer (ccl:fred-buffer w))
         (ed-prog (find-program w (ccl:buffer-position buffer)))
         prog new-prog)
    (unless ed-prog
      (error "Not inside a known program!"))
    (setq prog (program-of ed-prog))
    (multiple-value-bind (vars cleanup?) (realify-dialog prog)
      (declare (ignore vars))
      (setq new-prog (realify prog))
      (push `(new-prog (realify ,prog)) *transform-history*)
      (when cleanup? 
        (setq new-prog (cleanup-program new-prog))
        (push `(,new-prog (cleanup-program ,new-prog)) *transform-history*))
      (delete-program ed-prog)
      (insert-in-buffer w new-prog)
      (ccl:fred-update w))))

(defvar *realify-transform-dialog*)

(defun realify-dialog (program)
  (let ((cleanup? t)
        (all-cells (reverse (complex-valued-vars program)))
        (selected-objects nil)
        (unselected-objects nil)
        (*realify-transform-dialog* nil))
    
    (flet ((cell-update (item)
             (let* ((pt (first (ccl:selected-cells item)))
                    (cell (nth (ccl:point-v  pt) all-cells)))
               (cond ((member cell selected-objects)
                      (setq selected-objects (delete cell selected-objects))
                      (push cell unselected-objects))
                     (t 
                      (setq unselected-objects (delete cell unselected-objects))
                      (push cell selected-objects)))
               (ccl:redraw-cell item pt nil)
               (ccl:cell-deselect item pt nil))))
      
      (setq selected-objects (copy-list all-cells))
      
      (setq *realify-transform-dialog*          
            (make-instance 'ccl:window
              :window-type :double-edge-box
              :window-title "RealifyTransform"
              :view-position '(:TOP 60)
              :view-size #@(350 220)
              :close-box-p nil
              :view-font '("Chicago" 12 :srcor :plain)
              :view-subviews 
              (append 
               (list          
                (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                      #@(100 5) nil 
                                      "Realify Transform"
                                      'nil
                                      :view-font '("New Century Schlbk" 14 :srcor :bold)
                                      :part-color-list '(:text 2759876))
                
                (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                      #@(10 25) nil
                                      (format nil "Program: ~A" (name-of program))
                                      nil)
                
                (ccl:make-dialog-item 'ccl:sequence-dialog-item 
                                      #@(10 50) #@(230 130)
                                      "Table" #'cell-update
                                      :table-hscrollp nil
                                      :table-print-function 
                                      #'(lambda (obj stream) 
                                          (if (member obj selected-objects)
                                            (format stream "@ ~A" obj) 
                                            (format stream "   ~A" obj)))
                                      :table-sequence all-cells)
                
                (ccl:make-dialog-item 'ccl:check-box-dialog-item 
                                      #@(10 185) nil
                                      "Clean up afterwards?" 
                                      #'(lambda (item) 
                                          (setq cleanup? 
                                                (ccl:check-box-checked-p item)))
                                      :check-box-checked-p t)
                
                (ccl:make-dialog-item 'ccl:button-dialog-item 
                                      #@(260 160) #@(62 16) "Cancel" 
                                      #'(lambda (item)
                                          (declare (ignore item))
                                          (ccl:return-from-modal-dialog nil))
                                      :default-button nil)
                (ccl:make-dialog-item 'ccl:button-dialog-item 
                                      #@(250 190) #@(81 19) "Transform" 
                                      #'(lambda (item)
                                          (declare (ignore item))
                                          (ccl:return-from-modal-dialog t))
                                      :default-button t)))))    
      (when (ccl:modal-dialog *realify-transform-dialog*)
        (values selected-objects cleanup?)))))

;; Initial Value Transform interface 
(defun ed-initial-value-transform ()
  (let* ((w (ccl:front-window))
         (buffer (ccl:fred-buffer w))
         (ed-prog (find-program w (ccl:buffer-position buffer)))
         prog new-prog)
    (unless ed-prog
      (error "Not inside a known program!"))
    (setq prog (program-of ed-prog))
    (multiple-value-bind (ambient iv-transform stepping cleanup?)
        (iv-transform-dialog prog)
      (when ambient
        (setq new-prog 
              (iv-advance prog ambient iv-transform stepping))
        (when cleanup?
          (setq new-prog (cleanup-program new-prog)))
        (delete-program ed-prog)
        (insert-in-buffer w new-prog)
        (ccl:fred-update w)))))

(defvar *ambient-space* nil
  "Which ambient space to discretize")

(defvar *discretization-method* nil
  "Which IV discretization method to use")

(defvar *stepsizing-method* nil
  "Which stepsizing technique to use")

(defvar *iv-transform-dialog* nil)

(defun iv-transform-dialog (program)
  (let ((*ambient-space* nil)
        (*discretization-method* nil)
        (*stepsizing-method* nil)
        (*iv-transform-dialog*)
        (cleanup? t))
    (setq *iv-transform-dialog*          
          (make-instance 'ccl:window
            :window-type :double-edge-box
            :window-title "InitialValueTransform"
            :view-position '(:TOP 60)
            :view-size #@(350 220)
            :close-box-p nil
            :view-font '("Chicago" 12 :srcor :plain)
            :view-subviews 
            (append 
             (iv-discretization-popup-menu)
             (iv-stepsizing-menu)
             (iv-ambient-space-items program)
             (list          
              (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                    #@(90 5) nil 
                                    "Initial Value Transform"
                                    'nil
                                    :view-font '("New Century Schlbk" 14 :srcor :bold)
                                    :part-color-list '(:text 2759876))
              
              (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                    #@(10 25) nil
                                    (format nil "Program: ~A" (name-of program))
                                    nil)
              (ccl:make-dialog-item 'ccl:check-box-dialog-item 
                                    #@(10 185) nil
                                    "Clean up afterwards?" 
                                    #'(lambda (item) 
                                        (setq cleanup? 
                                              (ccl:check-box-checked-p item)))
                                    :check-box-checked-p t)
              
              (ccl:make-dialog-item 'ccl:button-dialog-item 
                                    #@(260 160) #@(62 16) "Cancel" 
                                    #'(lambda (item)
                                        (declare (ignore item))
                                        (ccl:return-from-modal-dialog nil))
                                    :default-button nil)
              (ccl:make-dialog-item 'ccl:button-dialog-item 
                                    #@(250 190) #@(81 19) "Transform" 
                                    #'(lambda (item)
                                        (declare (ignore item))
                                        (ccl:return-from-modal-dialog t))
                                    :default-button t)))))    
    (when (ccl:modal-dialog *iv-transform-dialog*)
      (let ((stepsize-parameter 
             (ccl:view-named 'stepsize-parameter *iv-transform-dialog*)))
        (when (ccl:dialog-item-enabled-p stepsize-parameter)
          (setq *stepsizing-method* *iv-constant-step*)
          (setf (step-size-of *stepsizing-method*)
                (read-from-string (ccl:dialog-item-text stepsize-parameter)))))
      (values *ambient-space*
              *discretization-method* *stepsizing-method*
              cleanup?))))

(defun iv-discretization-popup-menu ()
  ;; Setup the default method (last because we reverse the list!)
  (setq *discretization-method* (first (last *iv-transforms*)))
  (list 
   (ccl:make-dialog-item 'ccl:static-text-dialog-item
                         #@(10 50) #@(185 14)
                         "Discretization:" 'nil)
   (make-instance 'ccl:pop-up-menu
     :view-position  #@(165 50)
     :menu-items
     (loop for iv-transform in (reverse *IV-transforms*)
	   collect (make-menu-item 
                    (menu-name-of iv-transform)
                    ;; The following let is NECESSARY to capture the value
                    ;; of iv-transform!!!!!
                    (let ((transform iv-transform))
                      #'(lambda () 
                          (setq *discretization-method* transform))))))))

(defun iv-stepsizing-menu ()
  (let ((stepsize-parameter nil)
        (steppers (reverse *iv-steppers*)))
    ;; Setup the default step sizing method
    (setq *stepsizing-method* (nth 1 steppers))
    
    (list 
     (ccl:make-dialog-item 'ccl:static-text-dialog-item
                         #@(10 75) nil
                         "Step advancement:" 'nil)
     (make-instance 'ccl:pop-up-menu
       :view-position #@(165 75)
       :default-item 2
       :menu-items
       (cons
        (make-menu-item "Other Step Size"
                        #'(lambda ()
                            (ccl:dialog-item-enable stepsize-parameter))
                        :style :italic)
        (loop for stepper in steppers
              collect (make-menu-item 
                       (pretty-name-of stepper)
                       (let ((step stepper))
                         #'(lambda ()
                             (validate-stepper step)
                             (ccl:dialog-item-disable stepsize-parameter)                            
                             (setq *stepsizing-method* step)))))))
       (ccl:make-dialog-item 'ccl:static-text-dialog-item
                             #@(10 100) nil
                             "Step size:" 'nil)
       (setq stepsize-parameter
             (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                   #@(165 100) #@(80 15)
                                   ""
                                   nil
                                   :dialog-item-enabled-p nil
                                   :view-nick-name 'stepsize-parameter)))))


(defun iv-ambient-space-items (prog)
  (let ((spaces (ambient-spaces-of prog)))
    (when spaces
      (setq *ambient-space* (value-of (first spaces)))
      (loop for space in spaces
            for y-coord = 125 then (+ 25 y-coord)
            with x-coord = 10
            collect (ccl:make-dialog-item 
                     'ccl:radio-button-dialog-item
                     (ccl:make-point x-coord y-coord) nil
                     (format nil "Domain: ~S" (var-of space))
                     (let ((this-space space))
                       #'(lambda (item)
                         (declare (ignore item))
                         (setq *ambient-space* (value-of this-space))))
                     :radio-button-cluster 1)))))

(defun ed-cleanup-program () 
  (let* ((w (ccl:front-window))
         (buffer (ccl:fred-buffer w))
         (ed-prog (find-program w (ccl:buffer-position buffer)))
         new-prog)
    (unless ed-prog
      (error "Not inside a known program!"))
    (setq new-prog (cleanup-program (program-of ed-prog)))
    (delete-program ed-prog)
    (insert-in-buffer w new-prog)
    (ccl:fred-update w)))

;;; Solve equations

(defvar *equations-dialog*)

(defun ed-resolve-equations ()
  (let* ((w (ccl:front-window))
         (buffer (ccl:fred-buffer w))
         (ed-prog (find-program w (ccl:buffer-position buffer)))
         prog new-prog)
    (unless ed-prog
      (error "Not inside a known program!"))
    (setq prog (program-of ed-prog))
    (multiple-value-bind (transform cleanup?) 
                         (resolve-equations-dialog prog)
      (when transform
        (setq new-prog (funcall transform prog))
        (when cleanup? 
          (setq new-prog (cleanup-program new-prog)))
        (delete-program ed-prog)
        (insert-in-buffer w new-prog)
        (ccl:fred-update w)))))

(defun resolve-equations-dialog (program)
  (let ((cleanup? t)
        (transform 'solve-linearconstraints))
    (setq *equations-dialog*          
          (make-instance 'ccl:window
            :window-type :double-edge-box
            :window-title "Resolve Equations"
            :view-position '(:TOP 60)
            :view-size #@(350 220)
            :close-box-p nil
            :view-font '("Chicago" 12 :srcor :plain)
            :view-subviews 
            (list          
             (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                   #@(90 5) nil 
                                   "Resolve Equations"
                                   'nil
                                   :view-font '("New Century Schlbk" 14 :srcor :bold)
                                   :part-color-list '(:text 2759876))
             
             (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                   #@(10 25) nil
                                   (format nil "Program: ~A" (name-of program))
                                   nil)
             (ccl:make-dialog-item 'ccl:radio-button-dialog-item
                                   #@(10 50) nil
                                   "Explicitly solve linear equations"
                                   #'(lambda (item)
                                       (declare (ignore item))
                                       (setq transform 
                                             'solve-linearconstraints)))
             (ccl:make-dialog-item 'ccl:radio-button-dialog-item
                                   #@(10 75) nil
                                   "Triangularize equations"
                                   #'(lambda (item)
                                       (declare (ignore item))
                                       (setq transform 'solve-triangularize)))
             (ccl:make-dialog-item 'ccl:radio-button-dialog-item
                                   #@(10 100) nil
                                   "Resolve minimizations"
                                   #'(lambda (item)
                                       (declare (ignore item))
                                       (setq transform 'resolve-maximize)))
             (ccl:make-dialog-item 'ccl:radio-button-dialog-item
                                   #@(10 125) nil
                                   "Resolve maximizations"
                                   #'(lambda (item)
                                       (declare (ignore item))
                                       (setq transform 'resolve-maximize)))

             (ccl:make-dialog-item 'ccl:check-box-dialog-item 
                                    #@(10 185) nil
                                    "Clean up afterwards?" 
                                    #'(lambda (item) 
                                        (setq cleanup? 
                                              (ccl:check-box-checked-p item)))
                                    :check-box-checked-p t)

             (ccl:make-dialog-item 'ccl:button-dialog-item 
                                   #@(260 160) #@(62 16) "Cancel" 
                                   #'(lambda (item)
                                       (declare (ignore item))
                                       (ccl:return-from-modal-dialog nil))
                                   :default-button nil)
             (ccl:make-dialog-item 'ccl:button-dialog-item 
                                   #@(250 190) #@(81 19) "Transform" 
                                   #'(lambda (item)
                                       (declare (ignore item))
                                       (ccl:return-from-modal-dialog t))
                                   :default-button t))))
    (when (ccl:modal-dialog *equations-dialog*)
      (values transform cleanup?))))


(defun ed-boundary-value-transform ()
  (let* ((w (ccl:front-window))
         (buffer (ccl:fred-buffer w))
         (ed-prog (find-program w (ccl:buffer-position buffer)))
         prog new-prog)
    (unless ed-prog
      (error "Not inside a known program!"))
    (setq prog (program-of ed-prog))
    (multiple-value-bind (mesh meshing-params degree cleanup?)
                         (bvp-transform-dialog prog)
      (when mesh
        (let ((min-angle (first meshing-params))
              (max-angle (second meshing-params))
              (min-edge (third meshing-params))
              (max-edge (fourth meshing-params)))
          (discretize-mesh mesh `(,@(if (or min-angle max-angle)
                                      `(:angles '(,min-angle ,max-angle)))
                                  ,@(if min-edge
                                      `(:min-size ,min-edge))
                                  ,@(if max-edge
                                      `(:max-size ,max-edge))))))

      (setq new-prog (galerkin-transform prog degree))

      (when cleanup?
        (setq new-prog (cleanup-program new-prog)))
      (delete-program ed-prog)
      (insert-in-buffer w new-prog)
      (ccl:fred-update w))))

(defvar *bvp-transform-dialog* nil)

(defvar *mesh*)

(defvar *bvp-mesh-items* nil)

(defvar *bvp-degree-item* nil)

(defun bvp-transform-dialog (program)
  (let ((*mesh* nil)
        (cleanup? t))
    (setq *bvp-transform-dialog*          
          (make-instance 'ccl:window
            :window-type :double-edge-box
            :window-title "BoundaryValueTransform"
            :view-position '(:TOP 60)
            :view-size #@(350 220)
            :close-box-p nil
            :view-font '("Chicago" 12 :srcor :plain)
            :view-subviews 
            (append 
             (list          
              (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                    #@(40 5) nil 
                                    "Boundary Value Problem Transform"
                                    'nil
                                    :view-font '("New Century Schlbk" 14 :srcor :bold)
                                    :part-color-list '(:text 2759876))
              
              (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                    #@(10 25) nil
                                    (format nil "Program: ~A" (name-of program))
                                    nil)
              (ccl:make-dialog-item 'ccl:check-box-dialog-item 
                                    #@(10 185) nil
                                    "Clean up afterwards?" 
                                    #'(lambda (item) 
                                        (setq cleanup? 
                                              (ccl:check-box-checked-p item)))
                                    :check-box-checked-p t)
              
              (ccl:make-dialog-item 'ccl:button-dialog-item 
                                    #@(260 160) #@(62 16) "Cancel" 
                                    #'(lambda (item)
                                        (declare (ignore item))
                                        (ccl:return-from-modal-dialog nil))
                                    :default-button nil)
              (ccl:make-dialog-item 'ccl:button-dialog-item 
                                    #@(250 190) #@(81 19) "Transform" 
                                    #'(lambda (item)
                                        (declare (ignore item))
                                        (ccl:return-from-modal-dialog t))
                                    :default-button t))
             (bvp-mesh-items program))))
    (when (ccl:modal-dialog *bvp-transform-dialog*)
      (values *mesh*
              (loop for item in *bvp-mesh-items*
                    collect (and (ccl::dialog-item-enabled-p item)
                                 (read-from-string (ccl:dialog-item-text item)
                                                   nil)))
              (read-from-string (ccl:dialog-item-text *bvp-degree-item*)
                                nil)
              cleanup?))))

(defun bvp-mesh-items (prog)
  (let ((meshes (find-meshes prog))
        (x-coord 10)
        (y-coord 45)
        items item)
    (when meshes
      (setq *mesh* (value-of (first meshes)))
      (unless *bvp-mesh-items*
        (setq *bvp-mesh-items* (list nil nil nil nil)))
      
      (loop for mesh in meshes
            do
            (push (ccl:make-dialog-item 
                   'ccl:check-box-dialog-item
                   (ccl:make-point x-coord y-coord) nil
                   (format nil "Region: ~S" (var-of mesh))
                   (let ((this-mesh mesh))
                     #'(lambda (item)
                         (cond ((ccl::check-box-checked-p item)
                                (loop for item in *bvp-mesh-items*
                                      do (ccl::dialog-item-enable item)))
                               (t 
                                (loop for item in *bvp-mesh-items*
                                      do (ccl::dialog-item-disable item))))
                         (setq *mesh* (value-of this-mesh)))))
                  items)
            (incf y-coord 25))

      (setq item
            (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                  (ccl::make-point x-coord y-coord) #@(80 15)
                                  ""
                                  nil :dialog-item-enabled-p nil
                                  :view-nick-name 'min-angle))
      (push  item items)
      (setf (cl:first *bvp-mesh-items*) item)
      
      (push (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                  (ccl::make-point (+ x-coord 90) y-coord) nil
                                  "<= angle <=" nil)
            items)
      (setq item
            (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                  (ccl::make-point (+ x-coord 175) y-coord)
                                  #@(80 15)
                                  ""
                                  nil :dialog-item-enabled-p nil
                                  :view-nick-name 'max-angle))
      (push item items)
      (setf (cl:second *bvp-mesh-items*) item)
      (incf y-coord 25)

      (setq item
            (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                  (ccl::make-point x-coord y-coord) #@(80 15)
                                  ""
                                  nil :dialog-item-enabled-p nil
                                  :view-nick-name 'min-edge))
      (push item items)
      (setf (cl:third *bvp-mesh-items*) item)
      
      (push (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                  (ccl::make-point (+ x-coord 90) y-coord) nil
                                  "<= edge <=" nil)
            items)
      (setq item
            (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                  (ccl::make-point (+ x-coord 175) y-coord)
                                  #@(80 15)
                                  ""
                                  nil :dialog-item-enabled-p nil
                                  :view-nick-name 'max-edge))
      (push item items)
      (setf (cl:fourth *bvp-mesh-items*) item)
      (incf y-coord 25)

      (push (ccl:make-dialog-item 'ccl:static-text-dialog-item
                                  (ccl::make-point (+ x-coord) y-coord) nil
                                  "Shape function degree:" nil)
            items)
      (setq item
            (ccl:make-dialog-item 'ccl:editable-text-dialog-item
                                  (ccl::make-point (+ x-coord 175) y-coord)
                                  #@(20 15)
                                  "1"
                                  nil))
      (push item items)
      (setf *bvp-degree-item* item)
      (incf y-coord 25))
    items))

;;; Menu bar interface

(defun reset-menubar ()
  (ccl:set-menubar ccl:*default-menubar*))

(defvar *wrm-package* (find-package "WRM"))

(defclass transform-menu (ccl:menu)
  ())

(defmethod ccl:menu-update ((menu transform-menu))
  ;; If the current package is not WRM, then disable the transforms
  (if (eql (ccl:fred-package (ccl:front-window))
           *wrm-package*)
    (ccl:menu-enable menu)
    (ccl:menu-disable menu)))

(defun make-menu (title &rest items)
  (make-instance 'ccl:menu :menu-title title
                 :menu-items items))

(defun make-menu-item (title action &rest args)
  (apply #'make-instance
         'ccl:menu-item :menu-item-title title
         :menu-item-action action args))

(defun make-window-menu-item (title action &rest args)
  (apply #'make-instance 'ccl:window-menu-item :menu-item-title title
                 :menu-item-action action args))

(defvar *spl-transform-menu*)

(defun initialize-menubar ()
  (setq *spl-transform-menu* 
        (make-instance 'transform-menu :menu-title "SPL Transforms" 
          :help-spec "SPL/Weyl transformations on programs.  Only enabled when in a buffer in the WRM packge"
          :menu-items 
          (list (make-menu-item "Parse" 'ed-parse-spl-program)
                (make-menu-item "Cleanup Program" 'ed-cleanup-program)
                (make-menu-item "Realify" 'ed-realify-spl-program)
                (make-menu-item "Initial Value Problem" 'ed-initial-value-transform)
                (make-menu-item "Boundary Value Problem" 'ed-boundary-value-transform)
                (make-menu-item "Resolve Equations" 'ed-resolve-equations)
                (make-menu "Loops"
                           (make-menu-item "Unroll" nil)
                           (make-menu-item "Strength reduce" nil)))))
  (ccl:set-menubar (append ccl:*default-menubar* 
                           (list *spl-transform-menu*))))