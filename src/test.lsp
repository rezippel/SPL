;; Define the bitmap-window :Bitmap-Window-1
 
(in-package :common-lisp-user)
 
(defvar *bitmap-window-1* nil)
 
;; Return the window, creating it the first time or when it's closed.
;; Use only this function if you need only one instance.
(defun bitmap-window-1 ()
   (if (windowp *bitmap-window-1*) *bitmap-window-1* 
      (setq *bitmap-window-1* (make-bitmap-window-1))))
 
;; Create an instance of the window.
;; Use this if you need more than one instance.
(defun make-bitmap-window-1 (&key (parent *lisp-main-window*) 
                             (window-interior 
                                (make-box 442 116 891 575))
                             (name :bitmap-window-1) 
                             (title "Bitmap Window 1"))
   (setq *loaded-but-uncreated-windows* 
      (delete 'bitmap-window-1 *loaded-but-uncreated-windows*))
   (let (window-0 window-1 window-2 window-3 window-4)
      (setq window-0 
         (open-stream 'bitmap-window 
            parent :io 
            :name name 
            :title title 
            :font (make-font :swiss :system 16 '(:bold)) 
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
            :background-color white 
            :pop-up-p nil 
            :window-interior window-interior))
      (setf (window-editable-p window-0) t)
      (setf (getf (stream-plist window-0) :path) 
         (let* ((pathname (load-time-value *load-pathname*)))
            (if 
               (or (not (pathnamep pathname)) 
                  (member (pathname-type pathname) *fsl-extensions* 
                     :test #'string-equal))
               "C:\\rz\\Simlab\\spl\\src\\test.lsp" 
               (namestring pathname))))
      (setf (getf (stream-plist window-0) :startup-state) nil)
      (setf (getf (stream-plist window-0) :top-level-p) nil)
      (setf (help-string window-0) (delete #\Newline nil))
      (setf (getf (stream-plist window-0) :package) nil)
      (setq window-1 (frame-child window-0))
      nil
      (let* ((box (getf *window-exteriors* (object-name window-0))))
         (when box (reshape-window-exterior window-0 box)))
      (show-window window-0 nil)
      window-0))
(unless (windowp *bitmap-window-1*) 
   (pushnew 'bitmap-window-1 *loaded-but-uncreated-windows*))
