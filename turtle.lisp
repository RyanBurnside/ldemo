;;;; turt.lisp

;;; This turtle uses the short versions of LOGO commands
;;; NOTE unlike Berkeley LOGO, our turtle considers 0 degrees East ->
;;; This means 270 degrees is North for this turtle.
;;; Degrees increase clockwise from 0 to 360
;;; more here https://people.eecs.berkeley.edu/~bh/docs/html/usermanual_6.html#FORWARD

(in-package :ldemo)

(defclass turtle ()
  ((%x :initarg :x :accessor x :initform 0)
   (%y :initarg :y :accessor y :initform 0)
   (%direction :initarg :direction :accessor direction :initform 0)
   (%tail-down :initarg :tail-down :accessor tail-down :initform t)
   (%color :initarg :color :accessor color :initform "black")
   (%pen-size :initarg :pen-size :accessor pen-size :initform 1)
   (%draw-fn :initarg :draw-fn :accessor draw-fn :initform (constantly nil))))

(defun make-turtle (&rest rest)
  (apply #'make-instance 'turtle rest))


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

(defmethod bk ((turtle turtle) step)
  "Move backward calling the draw-fn if tail-down. No heading change."
  (fd turtle (- step)))

(defgeneric (setf direction) (value object))

(defmethod (setf direction) (degrees (turtle turtle))
  (setf (slot-value turtle '%direction) (wrap-degrees degrees)))

(defmethod home ((turtle turtle))
  "Move to [0, 0] and face 0 degrees."
  (seth turtle 0)
  (setpos turtle 0 0))

(defmethod rt ((turtle turtle) deg)
  "Turn right (clockwise)."
  (incf (direction turtle) (mod deg 360)))

(defmethod lt ((turtle turtle) deg)
  "Turn left (counter-lockwise)."
  (decf (direction turtle) (mod deg 360)))

(defmethod pu ((turtle turtle))
  "State: Raise the pen. Affects future fd and bk commands."
  (setf (tail-down turtle) nil))

(defmethod pd ((turtle turtle))
  "State: Lower the pen. Affects future fd and bk commands."
  (setf (tail-down turtle) t))

(defmethod seth ((turtle turtle) deg)
  "Set absolute heading."
  (setf (direction turtle) deg))

(defmethod setx ((turtle turtle) value)
  "Jump to x (no drawing)."
  (setf (x turtle) value))

(defmethod sety ((turtle turtle) value)
  "Jump to y (no drawing)."
  (setf (y turtle) value))

(defmethod setpc ((turtle turtle) value)
  "Set the color to value."
  (setf (color turtle) value))

(defmethod setpensize ((turtle turtle) value)
  (setf (pen-size turtle) value))

(defmethod setpos ((turtle turtle) x y)
  "Jump to [x, y] (no drawing)."
  (setx turtle x)
  (sety turtle y))

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
