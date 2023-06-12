(defpackage #:clickclackcloe
  (:use :cl)
  (:export :main))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "sdl2")
  (ql:quickload "cl-opengl")
  (ql:quickload "alexandria"))

(use-package :alexandria)

;; ----------------------------------------------------
(defvar *win-h* 600)
(defvar *win-w* 600)

(defun transpose-board (board)
  (destructuring-bind (n m) (array-dimensions board)
    (let ((new (make-array `(,n ,m) :initial-element nil)))
      (loop for i from 0 below n do
	(loop for j from 0 below m do
	  (setf (aref new j i)
		(aref board i j))))
      new)))

(defun board-diagonals (board)
  (let* ((n (array-dimension board 0)))
    (values
     (loop for i from 0 below n
       collect (aref board i i))
     (loop for i from (- n 1) downto 0
	   for j = (- n 1 i)
	   collect (aref board i j)))))

(defun iterate-board (board f)
  (destructuring-bind (n m) (array-dimensions board)
    (loop for i from 0 below n do
      (loop for j from 0 below m do
	(funcall f board i j)))))

(defun clear-board (board)
  (iterate-board board (lambda (b i j) (setf (aref b i j) nil))))

(defun between (a b x)
  (and (> x a) (< x b)))

(defun pos->index (x y)
  (flet ((ix (a)
	   (cond
	     ((between 0 200 a) 0)
	     ((between 200 400 a) 1)
	     ((between 400 600 a) 2))))
    (cons (ix x) (ix y))))

(defun all-equal-p (list)
  (loop for item in list always item))

(defun check-victory-rows (b n)
  (all-equal-p
   (loop for i from 0 below n
	 collect (loop for j from 0 below n
		       for p = (aref b i j)
		       when p
			 collect p))))

(defun check-victory (board)
  (let ((n (array-dimension board 0)))
    (or (check-victory-rows board n)
     
	(let ((transposed (transpose-board board)))
	  (check-victory-rows transposed n))
     
	(and (let ((mid (ceiling (/ (- n 1) 2)))) (aref board mid mid))
	     (multiple-value-bind (d1 d2) (board-diagonals board)
	       (or
		(all-equal-p d1)
		(all-equal-p d2)))))))

;; ----------------------------------------------------

(defun draw-circle (draw-function x0 y0 radius)
  (labels ((f (x y)
	     (funcall draw-function x y))
	   (put (x y m)
	     (let ((x+ (+ x0 x))
		   (x- (- x0 x))
		   (y+ (+ y0 y))
		   (y- (- y0 y))
		   (x0y+ (+ x0 y))
		   (x0y- (- x0 y))
		   (xy0+ (+ y0 x))
		   (xy0- (- y0 x)))
	       (f x+ y+)
	       (f x+ y-)
	       (f x- y+)
	       (f x- y-)
	       (f x0y+ xy0+)
	       (f x0y+ xy0-)
	       (f x0y- xy0+)
	       (f x0y- xy0-)
	       (multiple-value-bind (y m) (if (plusp m)
					      (values (1- y) (- m (* 8 y)))
					      (values y m))
		 (when (<= x y)
		   (put (1+ x)
			y
			(+ m 4 (* 8 x))))))))
    (put 0 radius (- 5 (* 4 radius)))
    (values)))

(defun draw-piece (renderer x0 y0 p)
  (case p
    (x
     (sdl2:render-draw-line renderer (- x0 50) (- y0 50) (+ x0 50) (+ y0 50))
     (sdl2:render-draw-line renderer (- x0 50) (+ y0 50) (+ x0 50) (- y0 50)))
    (o
     (draw-circle (lambda (x y) (sdl2:render-draw-point renderer x y)) x0 y0 50))))

(defun draw-board (renderer board)
  (let* ((h/3 (/ *win-h* 3))
	 (2h/3 (* 2 h/3)))
    (sdl2:render-draw-line renderer 0 h/3 *win-w* h/3)
    (sdl2:render-draw-line renderer 0 2h/3 *win-w* 2h/3)
    (sdl2:render-draw-line renderer h/3 0 h/3 *win-h*)
    (sdl2:render-draw-line renderer 2h/3 0 2h/3 *win-h*)

    (let ((mid (/ h/3 2)))
      (destructuring-bind (n m) (array-dimensions board)
	(loop for i from 0 below n do
	  (loop for j from 0 below m do
	    (when-let ((p (aref board i j)))
	      (let* ((x0 (+ mid (* i h/3)))
		     (y0 (+ mid (* j h/3))))
		(draw-piece renderer x0 y0 p)))))))))

;; ----------------------------------------------------

(defun render (renderer board)
  (sdl2:set-render-draw-color renderer #x00 #x00 #x00 #x00)
  (sdl2:render-clear renderer)
  (sdl2:set-render-draw-color renderer #xFF #xFF #xFF #xFF)
  (draw-board renderer board)
  (sdl2:render-present renderer))

(defmacro with-window-renderer ((window renderer) &body body)
  `(sdl2:with-init (:video)
     (sdl2:set-hint :render-scale-quality "1")
     (sdl2:with-window (,window
			:title "CLickCLackCloe"
			:w *win-w*
			:h *win-h*
			:flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
	 ,@body))))

(defun game-loop ()
  (let ((board (make-array '(3 3) :initial-element nil))
	(won nil))
    (flet ((update-pos (i j p)
	     (setf (aref board i j) p)
	     (when (check-victory board)
	       (setf won t)))
	   (reset-state ()
	     (clear-board board)
	     (setf won nil)))
      
      (with-window-renderer (window renderer)
	(sdl2:with-event-loop (:method :poll)
	  (:quit () t)
	  (:mousebuttondown ()
			    (multiple-value-bind (mx my btn) (sdl2:mouse-state)
			      (destructuring-bind (x . y) (pos->index mx my)
				(update-pos x y (case btn (1 'x) (4 'o))))))
	  (:idle ()
		 (render renderer board)

		 (when won
		   (sdl2-ffi.functions:sdl-show-simple-message-box
		    sdl2-ffi:+sdl-messagebox-information+
		    "Info" "You won" window)
		   (reset-state))
	       
		 (sdl2:delay 60)))))))

(defun main ()
  (game-loop))

(main)
