;;;; ldemo.lisp

(in-package #:ldemo)

(defun expand-path-old (iterations initial-string &rest replacement-rules)
  "Given an initial string and replacement rules recursivly iterate
   <initial-string> with rules sequentially <iterations> times. A rule
   is a pair with term to be replaced and its replacement."
  (dotimes (i iterations)
    (dolist (rule replacement-rules)
      (setf initial-string (str:replace-all (car rule)
                                            (cdr rule)
                                            initial-string))))
  initial-string)


(defun expand-path (iterations initial-string &rest replacement-rules)
  "Given an initial string and replacement rules recursivly iterate
   <initial-string> with rules sequentially <iterations> times. A rule
   is a pair with term to be replaced and its replacement."
  (flet ((lookup (char)
           (or (cdr (assoc char replacement-rules))
               char)))
    (dotimes (i iterations)
      (setf initial-string
            (format nil "~{~a~}" (map 'list #'lookup initial-string)))))
  initial-string)


(defun draw-line (canvas x y x2 y2 color pen-width)
  (make-line canvas `(,x ,y ,x2 ,y2) :fill color :width pen-width))

;;; Special to this demo
(defun draw-l-system (angle step turt l-string)
  (loop :with stack = `(,turt)
        :with current-turt = (first stack)
        :for i :across l-string :do
          (setf current-turt (first stack))
          (case i
            (#\+ (lt current-turt angle))
            (#\- (rt current-turt angle))
            (#\F (fd current-turt step))
            (#\[ (push (clone-turtle current-turt) stack))
            (#\] (pop stack)))))

(defun main ()
  (with-nodgui (:title "L System Toy")
    (set-geometry-wh *tk* 800 600)
    (let* ((sc (make-instance 'scrolled-canvas))
           (c (canvas sc))
           (draw-fn (lambda (&rest attribs)
                      (apply #'draw-line c attribs)))
           (turt (make-instance 'turtle :x 100 :y 600 :color "green" :draw-fn draw-fn))
           (l-string (expand-path 4 "F" '(#\F . "FF-[-F+F+F]+[+F-F-F]")))
           (turt2 (make-instance 'turtle :x 400 :y 600 :color "peru" :draw-fn draw-fn))
           (l-string2 (expand-path 7
                                   "X"
                                   '(#\X . "F[+X][-X]FX")
                                   '(#\F . "FF")))
           (turt3 (make-instance 'turtle :x 700 :y 600 :color "brown" :draw-fn draw-fn))
           (l-string3 (expand-path 5
                                   "X"
                                   '(#\X . "F-[[X]+X]+F[+FX]-X")
                                   '(#\F . "FF"))))
      (configure c :background "linen")
      (draw-l-system 22.5 7 turt l-string)
      (draw-l-system 25.7 2 turt2 l-string2)
      (draw-l-system 25.7 6 turt3 l-string3)
      (pack sc :expand 1 :fill :both)
      (scrollregion c 0 0 800 600))))


(export 'main)
