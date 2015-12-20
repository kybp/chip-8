(defpackage :chip-8
  (:use :cl))
(in-package :chip-8)

(defmacro aif (pred then else)
  `(let ((it ,pred))
     (if it ,then ,else)))

(defclass chip-8 ()
  ((ram       :initform (make-array 4096 :element-type '(unsigned-byte 8)))
   (registers :initform (make-array 16 :element-type '(unsigned-byte 8)))
   (i         :initform 0     :accessor i  :type (unsigned-byte 12))
   (pc        :initform #x200 :accessor pc :type (unsigned-byte 12))
   (screen    :initform (make-array 2048 :element-type 'bit)
              :reader   screen)
   (delay     :initform 0 :accessor delay-timer :type (unsigned-byte 8))
   (sound     :initform 0 :accessor sound-timer :type (unsigned-byte 8))
   (stack     :initform nil :accessor stack)
   (keys      :initform (make-array 16 :element-type 'boolean
                                    :initial-element nil))))

(defun make-chip-8 ()
  (make-instance 'chip-8))

(defun register (n chip-8)
  (aref (slot-value chip-8 'registers) n))

(defun (setf register) (value n chip-8)
  (setf (aref (slot-value chip-8 'registers) n)
        (mod value #x100))
  (values value (> value #xff)))

(defun ram (address chip-8)
  (aref (slot-value chip-8 'ram) address))

(defun (setf ram) (value address chip-8)
  (setf (aref (slot-value chip-8 'ram) address) value))

(defun key-down-p (n chip-8)
  (aref (slot-value chip-8 'keys) n))

(defun (setf key-down-p) (boolean n chip-8)
  (setf (aref (slot-value chip-8 'keys) n) boolean))

(defmacro recognizer (description)
  (loop for c across (reverse description)
     for offset from 0 by 4
     when (digit-char-p c 16)
     collect `(= (ldb (byte 4 ,offset) opcode) ,(digit-char-p c 16))
     into constants
     finally (return `(lambda (opcode) (and ,@constants)))))

(defmacro extracting-fn (description &body body)
  (let ((variables nil) (opcode (gensym)))
    (labels ((add-variable (c size offset)
               (push `(,(intern (string (char-upcase c)))
                       (ldb (byte ,size ,offset) ,opcode))
                     variables)))
      (loop with size = 0
         for c across (reverse description)
         and last = nil then c
         for offset from 0 by 4
         if (and (not (zerop size)) (not (eql c last))) do
           (add-variable last size (- offset size))
           (setf size (if (digit-char-p c 16) 0 4))
         else if (not (digit-char-p c 16)) do
           (incf size 4)
         finally (return `(lambda (,opcode chip-8)
                            (declare (ignorable ,opcode))
                            (let ,variables ,@body)))))))

(defmacro define-opcode (description &body body)
  `(push (cons (recognizer ,description)
               (extracting-fn ,description ,@body))
         *opcodes*))

(defparameter *opcodes* nil)

(define-opcode "00e0"
  (loop with screen = (screen chip-8)
     for i below (length screen) do
       (setf (aref screen i) 0)
     finally (incf (pc chip-8) 2)))

(define-opcode "00ee"
  (setf (pc chip-8) (pop (stack chip-8))))

(define-opcode "1nnn"
  (setf (pc chip-8) n))

(define-opcode "2nnn"
  (push (pc chip-8) (stack chip-8))
  (setf (pc chip-8) n))

(defun skip-when (skip-p chip-8)
  (incf (pc chip-8) (if skip-p 4 2)))

(define-opcode "3xnn"
  (skip-when (= (register x chip-8) n) chip-8))

(define-opcode "4xnn"
  (skip-when (/= (register x chip-8) n) chip-8))

(define-opcode "5xy0"
  (skip-when (= (register x chip-8) (register y chip-8)) chip-8))

(define-opcode "6xnn"
  (setf (register x chip-8) n)
  (incf (pc chip-8) 2))

(define-opcode "7xnn"
  (incf (register x chip-8) n)
  (incf (pc chip-8) 2))

(defun combine-registers (x y fn chip-8)
  (symbol-macrolet ((vx (register x chip-8)))
    (setf vx (funcall fn vx (register y chip-8)))
    (incf (pc chip-8) 2)))

(define-opcode "8xy0"
  (setf (register x chip-8) (register y chip-8))
  (incf (pc chip-8) 2))

(define-opcode "8xy1"
  (combine-registers x y #'logior chip-8))

(define-opcode "8xy2"
  (combine-registers x y #'logand chip-8))

(define-opcode "8xy3"
  (combine-registers x y #'logxor chip-8))

(define-opcode "8xy4"
  (let ((carryp (nth-value 2 (incf (register x chip-8) (register y chip-8)))))
    (setf (register #xf chip-8) (if carryp 1 0))
    (incf (pc chip-8) 2)))

(define-opcode "8xy6"
  (symbol-macrolet ((vy (register y chip-8)))
    (setf (register x   chip-8) (ash vy -1))
    (setf (register #xf chip-8) (ldb (byte 1 0) vy))
    (incf (pc chip-8) 2)))

(define-opcode "8xye"
  (symbol-macrolet ((vy (register y chip-8)))
    (setf (register x   chip-8) (ash vy 1))
    (setf (register #xf chip-8) (ldb (byte 1 7) vy))
    (incf (pc chip-8) 2)))

(define-opcode "9xy0"
  (skip-when (/= (register x chip-8) (register y chip-8)) chip-8))

(define-opcode "annn"
  (setf (i chip-8) n)
  (incf (pc chip-8) 2))

(define-opcode "bnnn"
  (setf (pc chip-8) (+ (register 0 chip-8) n)))

(define-opcode "cxnn"
  (setf (register x chip-8) (rand (logand (random #xff) n)))
  (incf (pc chip-8) 2))

(define-opcode "ex9e"
  (skip-when (key-down-p (register x chip-8) chip-8) chip-8))

(define-opcode "ex9e"
  (skip-when (not (key-down-p (register x chip-8) chip-8)) chip-8))

(define-opcode "fx07"
  (setf (register x chip-8) (delay-timer chip-8))
  (incf (pc chip-8) 2))

(define-opcode "fx15"
  (setf (delay-timer chip-8) (register x chip-8))
  (incf (pc chip-8) 2))

(define-opcode "fx18"
  (setf (sound-timer chip-8) (register x chip-8))
  (incf (pc chip-8) 2))

(define-opcode "fx1e"
  (incf (i chip-8) (register x chip-8))
  (incf (pc chip-8) 2))

(defun digits (n)
  (loop with digits = nil
     for x = n then (floor (/ x 10))
     until (zerop x) do
       (push (mod x 10) digits)
     finally (return digits)))

(define-opcode "fx33"
  (let ((digits  (digits (register x chip-8)))
        (address (i chip-8)))
    (dotimes (i 3)
      (setf (ram (+ address i) chip-8) (elt digits i)))))

(define-opcode "fx55"
  (loop for i from 0 to x
     do (setf (ram (i chip-8) chip-8) (register i chip-8))
       (incf (i chip-8))))

(define-opcode "fx65"
  (loop for i from 0 to x
     do (setf (register i chip-8) (ram (i chip-8) chip-8))
       (incf (i chip-8))))

(defun execute (opcode chip-8)
  (aif (cdr (assoc opcode *opcodes* :test #'(lambda (x fn) (funcall fn x))))
       (funcall it opcode chip-8)
       (error "Unrecognised instruction: #x~4,'0x" opcode)))
