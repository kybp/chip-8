(defpackage :chip-8-asd
  (:use :cl :asdf))
(in-package :chip-8-asd)

(defsystem :chip-8
  :description "CHIP-8 interpreter"
  :depends-on (:sdl2)
  :components ((:file "chip-8")))
