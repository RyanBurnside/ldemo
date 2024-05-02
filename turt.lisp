;;;; turt.lisp

;; minimal functions
;; more here https://people.eecs.berkeley.edu/~bh/docs/html/usermanual_6.html#FORWARD

;; NOTE unlike Berkeley LOGO, our turtle considers 0 degrees East ->
;; This means 270 degrees is North for this turtle.
;; Degrees increase clockwise from 0 to 360

;; This turtle uses the short versions of LOGO commands


(in-package :ldemo)

(defun dummy-fn (&rest args)
  "Harmless default function, consume arguments return NIL."
  (declare (ignore args))
  nil)

(defclass turtle ()
  ((x :initarg :x :accessor x :initform 0)
   (y :initarg :y :accessor y :initform 0)
   (direction :initarg :direction :accessor direction :initform 270)
   (tail-down :initarg :tail-down :accessor tail-down :initform t)
   (color :initarg :color :accessor color :initform "black")
   (pen-size :initarg :pen-size :accessor pen-size :initform 1)
   (draw-fn :initarg :draw-fn :accessor draw-fn :initform #'dummy-fn)))


(defun degrees->radians (degrees)
  (* degrees (/ pi 180)))


(defun wrap-degrees (degrees)
  (mod degrees 360))


(defmethod fd ((turtle turtle) step)
  "Move forward calling the draw-fn if tail-down."
  (let ((direction (degrees->radians (direction turtle)))
        (x1 (x turtle))
        (y1 (y turtle)))
    (incf (x turtle) (* (cos direction) step))
    (incf (y turtle) (* (sin direction) step))
    (when (tail-down turtle)
      (funcall (draw-fn turtle)
               x1 y1 (x turtle) (y turtle)
               (color turtle) (pen-size turtle)))))


(defmethod bk ((turt turtle) step)
  "Move backward calling the draw-fn if tail-down. No heading change."
  (fd turt (- step)))


(defgeneric (setf direction) (value object))


(defmethod (setf direction) (degrees (turt turtle))
  (setf (slot-value turt 'direction) (wrap-degrees degrees)))


(defmethod home ((turt turtle))
  "Move to [0, 0] and face 0 degrees."
  (seth turtle 0)
  (setpos turt 0 0))


(defmethod rt ((turt turtle) deg)
  "Turn right (clockwise)."
  (incf (direction turt) (mod deg 360)))


(defmethod lt ((turt turtle) deg)
  "Turn left (counter-lockwise)."
  (decf (direction turt) (mod deg 360)))


(defmethod pu ((turt turtle))
  "State: Raise the pen. Affects future fd and bk commands."
  (setf (tail-down turt) nil))


(defmethod pd ((turt turtle))
  "State: Lower the pen. Affects future fd and bk commands."
  (setf (tail-down turt) t))


(defmethod seth ((turt turtle) deg)
  "Set absolute heading."
  (setf (direction turt) deg))


(defmethod setx ((turt turtle) value)
  "Jump to x (no drawing)."
  (setf (x turt) value))


(defmethod sety ((turt turtle) value)
  "Jump to y (no drawing)."
  (setf (y turt) value))


(defmethod setpos ((turt turtle) x y)
  "Jump to [x, y] (no drawing)."
  (setx turt x)
  (sety turt y))


(defun clone-turtle (turtle)
  "Produce a copy of the turtle. draw-fn shared."
  (make-instance 'turtle
                 :x (x turtle)
                 :y (y turtle)
                 :direction (direction turtle)
                 :tail-down (tail-down turtle)
                 :color (color turtle)
                 :pen-size (pen-size turtle)
                 :draw-fn (draw-fn turtle)))


;; Should probably have a battery of tests very soon...
