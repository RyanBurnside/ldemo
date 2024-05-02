;;;; turt.lisp

;; minimal functions
;; more here https://people.eecs.berkeley.edu/~bh/docs/html/usermanual_6.html#FORWARD

(in-package :ldemo)

(defun dummy-fn (&rest args)
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
  (let ((direction (degrees->radians (direction turtle)))
        (x1 (x turtle))
        (y1 (y turtle)))
    (incf (x turtle) (* (cos direction) step))
    (incf (y turtle) (* (sin direction) step))
    (when (tail-down turtle)
      (funcall (draw-fn turtle)
               x1 y1 (x turtle) (y turtle)
               (color turtle) (pen-size turtle)))))


(defgeneric (setf direction) (value object))

(defmethod (setf direction) (degrees (turt turtle))
  (setf (slot-value turt 'direction) (wrap-degrees degrees)))


(defmethod rt ((turt turtle) deg)
  (incf (direction turt) (mod deg 360)))


(defmethod lt ((turt turtle) deg)
  (decf (direction turt) (mod deg 360)))


(defmethod pu ((turt turtle))
  (setf (tail-down turt) nil))


(defmethod pd ((turt turtle))
  (setf (tail-down turt) t))


(defun clone-turtle (turtle)
  (make-instance 'turtle
                 :x (x turtle)
                 :y (y turtle)
                 :direction (direction turtle)
                 :tail-down (tail-down turtle)
                 :color (color turtle)
                 :pen-size (pen-size turtle)
                 :draw-fn (draw-fn turtle)))
