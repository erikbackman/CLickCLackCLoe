(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "sdl2")
  (ql:quickload "cl-opengl")
  (ql:quickload "alexandria"))

(defpackage #:tictactoe
  (:use :cl :asdf)
  (:import-from :alexandria :when-let)
  (:export :main))

(in-package :tictactoe)

;; ----------------------------------------------------
(defparameter *win-h* 600)
(defparameter *win-w* 600)
(defparameter *cell-size* (/ *win-h* 3))

(defun transpose-board (board)
  (destructuring-bind (n m) (array-dimensions board)
    (let ((new (make-array `(,n ,m) :initial-element nil)))
      (loop for i from 0 below n do
	(loop for j from 0 below m do
	  (setf (aref new j i)
		(aref board i j))))
      new)))

(defun iterate-board (board f)
  (destructuring-bind (n m) (array-dimensions board)
    (loop for i from 0 below n do
      (loop for j from 0 below m do
	(funcall f board i j)))))

(defun pos->index (x y)
  (cons (floor (/ x *cell-size*))
	(floor (/ y *cell-size*))))

(defun check-victory-rows (board)
    (loop for i from 0 below 3
	  for fst = (aref board i 0)
	  thereis
	  (loop for j from 0 below 3
		for c = (aref board i j)
		always (and fst (equal fst c))
		finally (return fst))))

(defun check-victory (board)
  (or (check-victory-rows board)
      (check-victory-rows (transpose-board board))
      (let ((mid (aref board 1 1)))
	(and mid
	     (when (or (and (equal (aref board 0 0) mid)
			    (equal mid (aref board 2 2)))
		       (and (equal (aref board 2 0) mid)
			    (equal mid (aref board 0 2))))
	       mid)))))

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
    (:x
     (sdl2:render-draw-line renderer (- x0 50) (- y0 50) (+ x0 50) (+ y0 50))
     (sdl2:render-draw-line renderer (- x0 50) (+ y0 50) (+ x0 50) (- y0 50)))
    (:o
     (draw-circle (lambda (x y) (sdl2:render-draw-point renderer x y)) x0 y0 50))))

(defun draw-board (renderer board)
  (let* ((h/3 (/ *win-h* 3))
	 (2h/3 (* 2 h/3)))
    (sdl2:render-draw-line renderer 0 h/3 *win-w* h/3)
    (sdl2:render-draw-line renderer 0 2h/3 *win-w* 2h/3)
    (sdl2:render-draw-line renderer h/3 0 h/3 *win-h*)
    (sdl2:render-draw-line renderer 2h/3 0 2h/3 *win-h*)

    (let ((mid (/ h/3 2)))
      (iterate-board
       board
       (lambda (b i j) (when-let ((p (aref b i j)))
		    (let* ((x0 (+ mid (* i h/3)))
			   (y0 (+ mid (* j h/3))))
		      (draw-piece renderer x0 y0 p))))))))

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
			:title "tictactoe"
			:w *win-w*
			:h *win-h*
			:flags '(:shown))
       (sdl2:with-renderer (,renderer ,window :index -1 :flags '(:accelerated))
	 ,@body))))

(defclass game-state ()
    ((board :initform (make-array '(3 3) :initial-element nil) :accessor board)
     (winner :initform nil :accessor winner)))

(defmethod player-move ((obj game-state) x y p)
  (with-slots (board winner on-winner) obj
    (setf (aref board x y) p)
    (when-let ((winner? (check-victory board)))
      (setf winner winner?))))

(defmethod reset-game ((obj game-state))
  (with-slots (board winner) obj
    (iterate-board board (lambda (b i j) (setf (aref b i j) nil)))
    (setf winner nil)))

(defun game-loop ()
  (with-window-renderer (window renderer)
    (let ((state (make-instance 'game-state)))
      (flet ((re-render () (render renderer (board state)))
	     (show-winner (winner)
	       (sdl2-ffi.functions:sdl-show-simple-message-box
		sdl2-ffi:+sdl-messagebox-information+
		"Info" (format nil "~a won" winner) window)))

	(sdl2:with-event-loop (:method :poll)
	  (:quit () t)
	  (:keydown (:keysym keysym)
		    (case (sdl2:scancode keysym)
		      (:scancode-escape
		       (sdl2:push-event :quit))))
	  
	  (:mousebuttondown () (multiple-value-bind (mx my btn) (sdl2:mouse-state)
				 (declare (ignore btn))
				 (destructuring-bind (x . y) (pos->index mx my)
				   (player-move state x y :o))))

	  (:idle ()
		 (re-render)
		 (with-slots (winner) state
		   (when winner
		     (show-winner winner)
		     (reset-game state)))
		 (sdl2:delay 60)))))))

(defun main ()
  (game-loop))
