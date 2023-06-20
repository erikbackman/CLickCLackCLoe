(in-package :cl-user)
(defpackage tictactoe
  (:use :cl :asdf))
(in-package :tictactoe)

(defsystem tictactoe
  :name "tictactoe"
  :depends-on (:sdl2 :cl-opengl :alexandria)
  :components ((:file "main"))
  :build-operation "program-op"
  :build-pathname "tictactoe"
  :entry-point "tictactoe::main")
