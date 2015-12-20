(defpackage :chip-8
  (:use :cl :bordeaux-threads)
  (:export :main))
(in-package :chip-8)
(require :sdl2)

(defmacro aif (pred then else)
  `(let ((it ,pred))
     (if it ,then ,else)))

(defmacro awhen (pred &body body)
  `(let ((it ,pred))
     (when it ,@body)))

(defconstant +pixel-width+   10)
(defconstant +pixel-height+  10)
(defconstant +screen-width+  64)
(defconstant +screen-height+ 32)
(defconstant *on-rgba*  (list #xff #xff #x00 #x00))
(defconstant *off-rgba* (list #x00 #x64 #x00 #x00))

(defclass chip-8 ()
  ((ram        :initform (make-array 4096 :element-type '(unsigned-byte 8)))
   (registers  :initform (make-array 16   :element-type '(unsigned-byte 8)))
   (i          :initform 0     :accessor i  :type (unsigned-byte 16))
   (pc         :initform #x200 :accessor pc :type (unsigned-byte 12))
   (screen     :initform (make-array (list +screen-width+ +screen-height+)
                                     :element-type 'boolean
                                     :initial-element nil)
               :reader   screen)
   (renderer   :initarg :renderer :accessor renderer)
   (delay      :initform 0 :accessor delay-timer :type (unsigned-byte 8))
   (sound      :initform 0 :accessor sound-timer :type (unsigned-byte 8))
   (delay-lock :initform (make-lock) :accessor delay-lock)
   (sound-lock :initform (make-lock) :accessor sound-lock)
   (stack      :initform nil :accessor stack)
   (keys       :initform (make-array 16 :element-type 'boolean
                                     :initial-element nil))
   (wait-for-key :initform nil :accessor wait-for-key)))

(defun make-chip-8 (renderer)
  (let ((chip-8 (make-instance 'chip-8 :renderer renderer)))
    (make-thread #'(lambda () (update-timers chip-8)))
    (initialize-font chip-8)
    (clear-screen chip-8)
    chip-8))

(defun initialize-font (chip-8)
  (setf (ram #x50 chip-8 5) #xf0909090f0)  ; 0
  (setf (ram #x55 chip-8 5) #x2060202070)  ; 1
  (setf (ram #x5a chip-8 5) #xf010f080f0)  ; 2
  (setf (ram #x5f chip-8 5) #xf010f010f0)  ; 3
  (setf (ram #x64 chip-8 5) #x9090f01010)  ; 4
  (setf (ram #x69 chip-8 5) #xf080f010f0)  ; 5
  (setf (ram #x6e chip-8 5) #xf080f090f0)  ; 6
  (setf (ram #x73 chip-8 5) #xf010204040)  ; 7
  (setf (ram #x78 chip-8 5) #xf090f090f0)  ; 8
  (setf (ram #x7d chip-8 5) #xf090f010f0)  ; 9
  (setf (ram #x82 chip-8 5) #xf090f09090)  ; A
  (setf (ram #x87 chip-8 5) #xe090e090e0)  ; B
  (setf (ram #x8c chip-8 5) #xf0808080f0)  ; C
  (setf (ram #x91 chip-8 5) #xe0909090e0)  ; D
  (setf (ram #x96 chip-8 5) #xf080f080f0)  ; E
  (setf (ram #x9b chip-8 5) #xf080f08080)) ; F

(defun register (n chip-8)
  (aref (slot-value chip-8 'registers) n))

(defun (setf register) (value n chip-8)
  (setf (aref (slot-value chip-8 'registers) n)
        (mod value #x100))
  (values value (> value #xff)))

(defun ram (address chip-8 &optional (n 1))
  (loop with byte = 0
     for pos from 0 by 8
     for i from n above 0 do
       (setf (ldb (byte 8 pos) byte)
             (aref (slot-value chip-8 'ram) (+ address i)))
     finally (return byte)))

(defun (setf ram) (value address chip-8 &optional (n 1))
  (loop for pos from 0 by 8
     for i from n above 0 do
       (setf (aref (slot-value chip-8 'ram) (+ address i))
             (ldb (byte 8 pos) value))))

(defun key-down-p (n chip-8)
  (aref (slot-value chip-8 'keys) n))

(defun (setf key-down-p) (boolean n chip-8)
  (setf (aref (slot-value chip-8 'keys) n) boolean))

(defun pixel (screen x y)
  (aref screen (mod x +screen-width+) (mod y +screen-height+)))

(defun (setf pixel) (boolean screen x y)
  (setf (aref screen (mod x +screen-width+) (mod y +screen-height+)) boolean))

(defmacro define-opcode (description &body body)
  `(push (cons (recognizer ,description)
               (extracting-fn ,description ,body))
         *opcodes*))

(defmacro recognizer (description)
  (loop for c across (reverse description)
     for offset from 0 by 4
     for n = (digit-char-p c 16)
     when n collect `(= (ldb (byte 4 ,offset) opcode) ,n) into constants
     finally (return `(lambda (opcode) (and ,@constants)))))

(defmacro extracting-fn (description body)
  (loop with opcode = (gensym) and variables = nil and size = 0
     for c across (reverse description)
     and last = nil then c
     for offset from 0 by 4
     if (and (not (zerop size)) (not (eql c last))) do
       (push `(,(intern (string (char-upcase last)))
                (ldb (byte ,size ,(- offset size)) ,opcode))
             variables)
       (setf size (if (digit-char-p c 16) 0 4))
     else if (not (digit-char-p c 16)) do
       (incf size 4)
     finally (return `(lambda (,opcode chip-8)
                        (declare (ignorable ,opcode))
                        (let ,variables ,@body)))))

(defparameter *opcodes* nil)

(defun set-draw-color (renderer rgb)
  (eval `(sdl2:set-render-draw-color ,renderer ,@rgb)))

(defvar *rects* nil)

(defun draw-pixel (onp x y chip-8)
  (let ((renderer (renderer chip-8))
        (rect (sdl2:make-rect (* (mod x +screen-width+)  +pixel-width+)
                              (* (mod y +screen-height+) +pixel-height+)
                              +pixel-width+ +pixel-height+)))
    (set-draw-color renderer (if onp *on-rgba* *off-rgba*))
    (sdl2:render-fill-rect renderer rect)
    (push rect *rects*)))

(defun clear-screen (chip-8)
  (let ((screen (screen chip-8)))
    (dotimes (x +screen-width+)
      (dotimes (y +screen-height+)
        (setf (pixel screen x y) nil)))))

(defun draw-screen (chip-8)
  (let ((screen (screen chip-8)))
    (dotimes (x +screen-width+)
      (dotimes (y +screen-height+)
        (draw-pixel (pixel screen x y) x y chip-8)))
    (sdl2:render-present (renderer chip-8))
    (dolist (rect *rects*) (sdl2:free-rect rect))
    (setf *rects* nil)))

(define-opcode "00e0"
  (clear-screen chip-8)
  (incf (pc chip-8) 2))

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

(define-opcode "8xy5"
  (symbol-macrolet ((vx (register x chip-8))
                    (vy (register y chip-8)))
    (setf (register #xf chip-8) (if (> vy vx) 0 1))
    (setf vx (- vx vy))
    (incf (pc chip-8) 2)))

(define-opcode "8xy6"
  (symbol-macrolet ((vy (register y chip-8)))
    (setf (register x   chip-8) (ash vy -1))
    (setf (register #xf chip-8) (ldb (byte 1 0) vy))
    (incf (pc chip-8) 2)))

(define-opcode "8xy7"
  (symbol-macrolet ((vx (register x chip-8))
                    (vy (register y chip-8)))
    (setf (register #xf chip-8) (if (> vx vy) 0 1))
    (setf vx (- vy vx))
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
  (setf (register x chip-8) (logand (random #xff) n))
  (incf (pc chip-8) 2))

(defun get-sprite (address n chip-8)
  (loop for i from 0 below n
     collect (map 'list #'(lambda (c) (eql #\1 c))
                  (format nil "~8,'0b" (ram (+ address i) chip-8)))))

(define-opcode "dxyn"
  (loop with vx = (register x chip-8) and vy = (register y chip-8)
     and sprite = (get-sprite (i chip-8) n chip-8)
     for i below n
     for row = (elt sprite i)
     do (loop for pixel in row for j from vx
           do (symbol-macrolet ((screen (pixel (screen chip-8) j (+ i vy))))
                (when pixel
                  (when screen (setf (register #xf chip-8) 1))
                  (setf screen (not screen))
                  (draw-pixel screen j (+ i vy) chip-8))))
     finally (incf (pc chip-8) 2)))

(define-opcode "ex9e"
  (skip-when (key-down-p (register x chip-8) chip-8) chip-8))

(define-opcode "exa1"
  (skip-when (not (key-down-p (register x chip-8) chip-8)) chip-8))

(define-opcode "fx07"
  (with-lock-held ((delay-lock chip-8))
    (setf (register x chip-8) (delay-timer chip-8)))
  (incf (pc chip-8) 2))

(define-opcode "fx0a"
  (setf (wait-for-key chip-8) x)
  (incf (pc chip-8) 2))

(define-opcode "fx15"
  (with-lock-held ((delay-lock chip-8))
    (setf (delay-timer chip-8) (register x chip-8)))
  (incf (pc chip-8) 2))

(define-opcode "fx18"
  (with-lock-held ((sound-lock chip-8))
    (setf (sound-timer chip-8) (register x chip-8)))
  (incf (pc chip-8) 2))

(define-opcode "fx1e"
  (incf (i chip-8) (register x chip-8))
  (incf (pc chip-8) 2))

(define-opcode "fx29"
  (setf (i chip-8) (+ (* 5 (register x chip-8)) #x50))
  (incf (pc chip-8) 2))

(defun digits (n)
  (loop with digits = nil
     for x = n then (floor (/ x 10))
     until (zerop x) do
       (push (mod x 10) digits)
     finally (return (dotimes (i (- 3 (length digits)) digits)
                       (push 0 digits)))))

(define-opcode "fx33"
  (let ((digits  (digits (register x chip-8)))
        (address (i chip-8)))
    (dotimes (i 3)
      (setf (ram (+ address i) chip-8) (elt digits i))
      (incf (pc chip-8) 2))))

(define-opcode "fx55"
  (loop for i from 0 to x
     do (setf (ram (i chip-8) chip-8) (register i chip-8))
       (incf (i chip-8))
     finally (incf (pc chip-8) 2)))

(define-opcode "fx65"
  (loop for i from 0 to x
     do (setf (register i chip-8) (ram (i chip-8) chip-8))
       (incf (i chip-8))
     finally (incf (pc chip-8) 2)))

(defun fetch-instruction (chip-8)
  (ram (pc chip-8) chip-8 2))

(defun execute (opcode chip-8)
  (aif (cdr (assoc opcode *opcodes* :test #'(lambda (x fn) (funcall fn x))))
       (funcall it opcode chip-8)
       (warn "Unrecognised instruction: #x~4,'0x" opcode)))

(defun main (file)
  (let ((width  (* +pixel-width+  +screen-width+))
        (height (* +pixel-height+ +screen-height+)))
    (sdl2:with-init ()
      (sdl2:with-window (win :title "CHIP-8" :w width :h height)
        (sdl2:with-renderer (renderer win :flags '(:accelerated))
          (let ((chip-8 (make-chip-8 renderer)))
            (load-file file chip-8)
            (sdl2:with-event-loop ()
              (:keydown
               (:keysym keysym)
               (let* ((scancode (sdl2:scancode-value keysym))
                      (hex      (hex-key-from-scancode scancode)))
                 (awhen (and hex (wait-for-key chip-8))
                   (setf (wait-for-key chip-8) nil)
                   (setf (register it chip-8) hex))
                 (handle-key-down keysym chip-8)))
              (:keyup
               (:keysym keysym)
               (handle-key-up keysym chip-8))
              (:quit () t)
              (:idle
               ()
               (unless (wait-for-key chip-8)
                 (execute (fetch-instruction chip-8) chip-8))
               (draw-screen chip-8)))))))))

(defun handle-key-down (keysym chip-8)
  (let* ((scancode (sdl2:scancode-value keysym))
         (hex      (hex-key-from-scancode scancode)))
    (cond ((sdl2:scancode= scancode :scancode-escape)
           (sdl2:push-event :quit))
          (hex (setf (key-down-p hex chip-8) t)))))

(defun handle-key-up (keysym chip-8)
  (awhen (hex-key-from-scancode (sdl2:scancode-value keysym))
    (setf (key-down-p it chip-8) nil)))

(defun hex-key-from-scancode (scancode)
  (cond ((sdl2:scancode= scancode :scancode-1) #x1)
        ((sdl2:scancode= scancode :scancode-2) #x2)
        ((sdl2:scancode= scancode :scancode-3) #x3)
        ((sdl2:scancode= scancode :scancode-4) #xc)
        ((sdl2:scancode= scancode :scancode-q) #x4)
        ((sdl2:scancode= scancode :scancode-w) #x5)
        ((sdl2:scancode= scancode :scancode-e) #x6)
        ((sdl2:scancode= scancode :scancode-r) #xd)
        ((sdl2:scancode= scancode :scancode-a) #x7)
        ((sdl2:scancode= scancode :scancode-s) #x8)
        ((sdl2:scancode= scancode :scancode-d) #x9)
        ((sdl2:scancode= scancode :scancode-f) #xe)
        ((sdl2:scancode= scancode :scancode-z) #xa)
        ((sdl2:scancode= scancode :scancode-x) #x0)
        ((sdl2:scancode= scancode :scancode-c) #xb)
        ((sdl2:scancode= scancode :scancode-v) #xf)))

(defun load-file (file chip-8)
  (cond ((equal (pathname-type file) "txt")
         (load-text-file file chip-8))
        ((equal (pathname-type file) "ch8")
         (load-binary-file file chip-8))
        (t (error "Unrecognised file type for ~s" file))))

(defun load-text-file (file chip-8)
  (with-open-file (source file)
    (loop for line = (read-line source nil nil)
       for pc from #x200 by 2
       while line do
         (setf (ram pc chip-8 2)
               (parse-integer (subseq line 0 4) :radix 16)))))

(defun load-binary-file (file chip-8)
  (with-open-file (input file :element-type '(unsigned-byte 8))
    (loop for pc from #x200
       for byte = (read-byte input nil nil)
       while byte do (setf (ram pc chip-8) byte))))

(defun update-timers (chip-8)
  (sleep (/ 1 60))
  (with-lock-held ((delay-lock chip-8))
    (when (> (delay-timer chip-8) 0)
      (decf (delay-timer chip-8))))
  (with-lock-held ((sound-lock chip-8))
    (when (> (sound-timer chip-8) 0)
      (decf (sound-timer chip-8))))
  (update-timers chip-8))
