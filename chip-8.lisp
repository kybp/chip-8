(defpackage :chip-8
  (:use :cl :bordeaux-threads)
  (:export :main))
(in-package :chip-8)
(require :sdl2)

;;; Util

(defmacro aif (pred then else)
  `(let ((it ,pred))
     (if it ,then ,else)))

(defmacro awhen (pred &body body)
  `(let ((it ,pred))
     (when it ,@body)))

;;; Constants

(defparameter *pixel-width* 10
  "The width in actual screen pixels of 1 CHIP-8 pixel.")

(defparameter *pixel-height* 10
  "The height in actual screen pixels of 1 CHIP-8 pixel.")

(defparameter *screen-width* 64
  "The width in CHIP-8 pixels of a CHIP-8 screen.")

(defparameter *screen-height* 32
  "The height in CHIP-8 pixels of a CHIP-8 screen.")

(defvar *on-rgba* (list #xff #xff #x00 #x00)
  "A list of 4 16-bit values indicating the RGBA color to draw for a CHIP-8
pixel when that pixel is set.")

(defvar *off-rgba* (list #x00 #x64 #x00 #x00)
  "A list of 4 16-bit values indicating the RGBA color to draw for a CHIP-8
pixel when that pixel is not set.")

;;; CHIP-8 interface

(defstruct pixel on-p rect)

(defun make-screen ()
  (let ((screen (make-array
                 (list *screen-width* *screen-height*)
                 :element-type 'pixel)))
    (dotimes (x *screen-width* screen)
      (dotimes (y *screen-height*)
        (let* ((x* (* (mod x *screen-width*)  *pixel-width*))
               (y* (* (mod y *screen-height*) *pixel-height*))
               (rect (sdl2:make-rect x* y* *pixel-width* *pixel-height*)))
          (setf (aref screen x y) (make-pixel :on-p nil :rect rect)))))))

(defun free-screen (screen)
  (dotimes (x *screen-width*)
    (dotimes (y *screen-height*)
      (sdl2:free-rect (pixel-rect (aref screen x y))))))

(defclass chip-8 ()
  ((ram        :initform (make-array 4096 :element-type '(unsigned-byte 8)))
   (registers  :initform (make-array 16   :element-type '(unsigned-byte 8)))
   (i          :initform 0     :accessor i  :type (unsigned-byte 16))
   (pc         :initform #x200 :accessor pc :type (unsigned-byte 12))
   (screen     :initform (make-screen) :reader   screen)
   (renderer   :initarg :renderer :accessor renderer)
   (delay      :initform 0 :accessor delay-timer :type (unsigned-byte 8))
   (sound      :initform 0 :accessor sound-timer :type (unsigned-byte 8))
   (delay-lock :initform (make-lock) :accessor delay-lock)
   (sound-lock :initform (make-lock) :accessor sound-lock)
   (stack      :initform nil :accessor stack)
   (keys       :initform
               (make-array 16 :element-type 'boolean :initial-element nil))
   (wait-for-key :initform nil :accessor wait-for-key)
   (timer-thread :initform nil :accessor timer-thread)))

(defun make-chip-8 (renderer)
  "Create a new CHIP-8 instance."
  (let ((chip-8 (make-instance 'chip-8 :renderer renderer)))
    (setf (timer-thread chip-8)
          (make-thread #'(lambda () (update-timers chip-8))))
    (initialize-font chip-8)
    (clear-screen chip-8)
    chip-8))

(defun free-chip-8 (chip-8)
  (awhen (timer-thread chip-8)
    (destroy-thread it))
  (free-screen (screen chip-8)))

(defun initialize-font (chip-8)
  "Store font data for the 16 hex characters 0-F in the given CHIP-8."
  (loop
     with bytes-per-character = 5
     and characters = '(#xf0909090f0 #x2060202070 #xf010f080f0 #xf010f010f0
                        #x9090f01010 #xf080f010f0 #xf080f090f0 #xf010204040
                        #xf090f090f0 #xf090f010f0 #xf090f09090 #xe090e090e0
                        #xf0808080f0 #xe0909090e0 #xf080f080f0 #xf080f08080)
     for address from #x50 by bytes-per-character
     and character in characters
     do (setf (ram address chip-8 bytes-per-character) character)))

(defun update-timers (chip-8)
  "Loop indefinitely, ticking the timers in CHIP-8."
  (loop
     (sleep (/ 1 60))
     (with-lock-held ((delay-lock chip-8))
       (when (> (delay-timer chip-8) 0)
         (decf (delay-timer chip-8))))
     (with-lock-held ((sound-lock chip-8))
       (when (> (sound-timer chip-8) 0)
         (decf (sound-timer chip-8))))))

(defun register (n chip-8)
  "Return the value of register N in the given CHIP-8."
  (aref (slot-value chip-8 'registers) n))

(defun (setf register) (value n chip-8)
  (setf (aref (slot-value chip-8 'registers) n)
        (mod value #x100))
  (values value (> value #xff)))

(defun ram (address chip-8 &optional (n 1))
  "Return N bytes from RAM in the given CHIP-8, beginning at ADDRESS."
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
  "Return whether or not the N key is currently down in CHIP-8. N should be a
positive integer less than 16."
  (aref (slot-value chip-8 'keys) n))

(defun (setf key-down-p) (boolean n chip-8)
  (setf (aref (slot-value chip-8 'keys) n) boolean))

(defun pixel (screen x y)
  "Return the value of the pixel at (X, Y) on SCREEN. This will be a boolean
indicating whether that pixel is on or not."
  (aref screen (mod x *screen-width*) (mod y *screen-height*)))

(defun fetch-instruction (chip-8)
  "Return the opcode of the next instruction to be executed in CHIP-8."
  (ram (pc chip-8) chip-8 2))

(defun execute (opcode chip-8)
  "Execute the given OPCODE in the given CHIP-8. If OPCODE cannot be decoded to
a valid instruction, signal a warning."
  (aif (cdr (assoc opcode *opcodes* :test #'(lambda (x fn) (funcall fn x))))
       (funcall it opcode chip-8)
       (warn "Unrecognised instruction: #x~4,'0x" opcode)))

;;; Opcode definitions

(defmacro define-opcode (description &body body)
  "Define a new opcode in the ISA.

DESCRIPTION should be a string describing the opcode's hex format. Valid hex
digits will be interpreted as literal numbers, while invalid hex digits will be
treated as parameters and provided for use in BODY.

BODY is the code to be executed when the opcode is executed. During its
execution, parameters given in DESCRIPTION will be provided, and the CHIP-8
instance will be provided as CHIP-8."
  `(push (cons (recognizer ,description)
               (extracting-fn ,description ,body))
         *opcodes*))

(defmacro recognizer (description)
  "Return a function that when called with an opcode will return a boolean
indicating whether or not it matches DESCRIPTION (see DEFINE-OPCODE for the
format DESCRIPTION should be in)."
  (loop for c across (reverse description)
     for offset from 0 by 4
     for n = (digit-char-p c 16)
     when n collect `(= (ldb (byte 4 ,offset) opcode) ,n) into constants
     finally (return `(lambda (opcode) (and ,@constants)))))

(defmacro extracting-fn (description body)
  "Return a function that will wrap the list of forms BODY with code for
extracting the variables described in DESCRIPTION and providing them as
variables during its execution (see DEFINE-OPCODE for further information)."
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

(defparameter *opcodes* nil
  "An alist representing the CHIP-8 ISA. The keys are functions accepting an
opcode and returning a boolean indicating whether that instruction matches the
given opcode. The values are functions accepting an opcode and a CHIP-8 instance
which when called will execute that instruction. Additions to this list should
be made using DEFINE-OPCDOE.")

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

(defun next-instruction (chip-8)
  "Advance the PC in CHIP-8 to the next instruction."
  (incf (pc chip-8) 2))

(defun skip-when (skip-p chip-8)
  "If SKIP-P is non-NIL, then skip the next instruction, otherwise don't."
  (incf (pc chip-8) (if skip-p 4 2)))

(define-opcode "3xnn"
  (skip-when (= (register x chip-8) n) chip-8))

(define-opcode "4xnn"
  (skip-when (/= (register x chip-8) n) chip-8))

(define-opcode "5xy0"
  (skip-when (= (register x chip-8) (register y chip-8)) chip-8))

(define-opcode "6xnn"
  (setf (register x chip-8) n)
  (next-instruction chip-8))

(define-opcode "7xnn"
  (incf (register x chip-8) n)
  (next-instruction chip-8))

(define-opcode "8xy0"
  (setf (register x chip-8) (register y chip-8))
  (next-instruction chip-8))

(defun combine-registers (x y fn chip-8)
  "Set register X in CHIP-8 to the result of applying FN to the values of
registers X and Y."
  (symbol-macrolet ((vx (register x chip-8)))
    (setf vx (funcall fn vx (register y chip-8)))
    (next-instruction chip-8)))

(define-opcode "8xy1"
  (combine-registers x y #'logior chip-8))

(define-opcode "8xy2"
  (combine-registers x y #'logand chip-8))

(define-opcode "8xy3"
  (combine-registers x y #'logxor chip-8))

(define-opcode "8xy4"
  (let ((carryp (nth-value 2 (incf (register x chip-8) (register y chip-8)))))
    (setf (register #xf chip-8) (if carryp 1 0))
    (next-instruction chip-8)))

(define-opcode "8xy5"
  (symbol-macrolet ((vx (register x chip-8))
                    (vy (register y chip-8)))
    (setf (register #xf chip-8) (if (> vy vx) 0 1))
    (setf vx (- vx vy))
    (next-instruction chip-8)))

(define-opcode "8xy6"
  (symbol-macrolet ((vy (register y chip-8)))
    (setf (register x   chip-8) (ash vy -1))
    (setf (register #xf chip-8) (ldb (byte 1 0) vy))
    (next-instruction chip-8)))

(define-opcode "8xy7"
  (symbol-macrolet ((vx (register x chip-8))
                    (vy (register y chip-8)))
    (setf (register #xf chip-8) (if (> vx vy) 0 1))
    (setf vx (- vy vx))
    (next-instruction chip-8)))

(define-opcode "8xye"
  (symbol-macrolet ((vy (register y chip-8)))
    (setf (register x   chip-8) (ash vy 1))
    (setf (register #xf chip-8) (ldb (byte 1 7) vy))
    (next-instruction chip-8)))

(define-opcode "9xy0"
  (skip-when (/= (register x chip-8) (register y chip-8)) chip-8))

(define-opcode "annn"
  (setf (i chip-8) n)
  (next-instruction chip-8))

(define-opcode "bnnn"
  (setf (pc chip-8) (+ (register 0 chip-8) n)))

(define-opcode "cxnn"
  (setf (register x chip-8) (logand (random #xff) n))
  (next-instruction chip-8))

(defun get-sprite (address n chip-8)
  "Return a list of lists representing the pixels in the sprite N rows high
stored at ADDRESS in CHIP-8. Each list in the result represents one row of
pixels."
  (loop for i from 0 below n
     collect (map 'list #'(lambda (c) (eql #\1 c))
                  (format nil "~8,'0b" (ram (+ address i) chip-8)))))

(define-opcode "dxyn"
  (loop with vx = (register x chip-8) and vy = (register y chip-8)
     and sprite = (get-sprite (i chip-8) n chip-8)
     for i below n
     for row = (elt sprite i)
     do (loop for pixel in row for j from vx
           do (symbol-macrolet ((pixel-on-p
                                 (pixel-on-p
                                  (pixel (screen chip-8) j (+ i vy)))))
                (when pixel
                  (when pixel-on-p (setf (register #xf chip-8) 1))
                  (setf pixel-on-p (not pixel-on-p)))))
     finally (next-instruction chip-8)))

(define-opcode "ex9e"
  (skip-when (key-down-p (register x chip-8) chip-8) chip-8))

(define-opcode "exa1"
  (skip-when (not (key-down-p (register x chip-8) chip-8)) chip-8))

(define-opcode "fx07"
  (with-lock-held ((delay-lock chip-8))
    (setf (register x chip-8) (delay-timer chip-8)))
  (next-instruction chip-8))

(define-opcode "fx0a"
  (setf (wait-for-key chip-8) x)
  (next-instruction chip-8))

(define-opcode "fx15"
  (with-lock-held ((delay-lock chip-8))
    (setf (delay-timer chip-8) (register x chip-8)))
  (next-instruction chip-8))

(define-opcode "fx18"
  (with-lock-held ((sound-lock chip-8))
    (setf (sound-timer chip-8) (register x chip-8)))
  (next-instruction chip-8))

(define-opcode "fx1e"
  (incf (i chip-8) (register x chip-8))
  (next-instruction chip-8))

(define-opcode "fx29"
  (setf (i chip-8) (+ (* 5 (register x chip-8)) #x50))
  (next-instruction chip-8))

(defun digits (n)
  "Return a list of the base-10 digits in the positive integer N."
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
      (next-instruction chip-8))))

(define-opcode "fx55"
  (loop for i from 0 to x
     do (setf (ram (i chip-8) chip-8) (register i chip-8))
       (incf (i chip-8))
     finally (next-instruction chip-8)))

(define-opcode "fx65"
  (loop for i from 0 to x
     do (setf (register i chip-8) (ram (i chip-8) chip-8))
       (incf (i chip-8))
     finally (next-instruction chip-8)))

;;; Drawing

(defun set-draw-color (renderer rgb)
  (eval `(sdl2:set-render-draw-color ,renderer ,@rgb)))

(defun draw-pixel (x y chip-8)
  (let ((renderer (renderer chip-8))
        (pixel    (pixel (screen chip-8) x y)))
    (set-draw-color renderer (if (pixel-on-p pixel) *on-rgba* *off-rgba*))
    (sdl2:render-fill-rect renderer (pixel-rect pixel))))

(defun clear-screen (chip-8)
  "Un-set all of the pixels in CHIP-8's screen."
  (let ((screen (screen chip-8)))
    (dotimes (x *screen-width*)
      (dotimes (y *screen-height*)
        (setf (pixel-on-p (pixel screen x y)) nil)))))

(defun finish-draw (chip-8)
  "Finalize the current frame."
  (sdl2:render-present (renderer chip-8)))

(defun draw-screen (chip-8)
  "Draw the current screen state to the SDL window."
  (let ((screen (screen chip-8)))
    (dotimes (x *screen-width*)
      (dotimes (y *screen-height*)
        (draw-pixel x y chip-8)))
    (finish-draw chip-8)))

;;; Event handling

(defun handle-key-down (keysym chip-8)
  "If KEYSYM corresponds to one of the 16 CHIP-8 keys, mark that key as being
pressed down. If KEYSYM corresponds to the escape key, exit the program.
Otherwise, do nothing."
  (let* ((scancode (sdl2:scancode-value keysym))
         (hex      (hex-key-from-scancode scancode)))
    (cond ((sdl2:scancode= scancode :scancode-escape)
           (sdl2:push-event :quit))
          (hex (setf (key-down-p hex chip-8) t)))))

(defun handle-key-up (keysym chip-8)
  (awhen (hex-key-from-scancode (sdl2:scancode-value keysym))
    (setf (key-down-p it chip-8) nil)))

(defun hex-key-from-scancode (scancode)
  "Return the integer between 0 and 15 inclusive corresponding to SCANCODE. If
SCANCODE does not correspond to any of the 16 keys, return NIL instead."
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

;;; File handling

(defun load-file (file chip-8)
  "Load FILE into memory on CHIP-8."
  (cond ((equal (pathname-type file) "txt")
         (load-text-file file chip-8))
        ((equal (pathname-type file) "ch8")
         (load-binary-file file chip-8))
        (t (error "Unrecognised file type for ~s" file))))

(defun load-text-file (filespec chip-8)
  "Load the file indicated by FILESPEC into memory on CHIP-8, treating the file
as a text file containing ASCII representations of the instructions to load."
  (with-open-file (source filespec)
    (loop for line = (read-line source nil nil)
       for pc from #x200 by 2
       while line do
         (setf (ram pc chip-8 2)
               (parse-integer (subseq line 0 4) :radix 16)))))

(defun load-binary-file (filespec chip-8)
  "Load the file indicated by FILESPEC into memory on CHIP-8, treating the file
as a binary file."
  (with-open-file (input filespec :element-type '(unsigned-byte 8))
    (loop for pc from #x200
       for byte = (read-byte input nil nil)
       while byte do (setf (ram pc chip-8) byte))))

(defun run-chip-8 (chip-8)
  "Run the CHIP-8 simulator in an SDL event loop until the user exits."
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
       (execute (fetch-instruction chip-8) chip-8)
       (draw-screen chip-8)))))

(defun main (filespec)
  "Create a new CHIP-8 instance and graphical window and attempt to run the file
indicated by FILESPEC."
  (let ((width  (* *pixel-width*  *screen-width*))
        (height (* *pixel-height* *screen-height*)))
    (sdl2:with-init ()
      (sdl2:with-window (window :title "CHIP-8" :w width :h height)
        (sdl2:with-renderer (renderer window :flags '(:accelerated))
          (let ((chip-8 (make-chip-8 renderer)))
            (load-file filespec chip-8)
            (unwind-protect (run-chip-8 chip-8)
              (free-chip-8 chip-8))))))))
