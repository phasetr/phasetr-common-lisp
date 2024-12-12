;;; P.13
(when (> (length vec) 0)
  (let ((beg 0)
        (end (length vec)))
    (do ()
        ((= beg end))
      (let ((mid (floor (+ beg end) 2)))
        (if (> (aref vec mid) val)
            (setf end mid)
            (setf beg (1+ mid)))))
    (values beg
            (aref vec beg)
            (= (aref vec beg) val))))

;;; P.15
(print "Hello world")
(+ 2 2)

;;; P.16
(block test
  (return-from test 0)
  (print "hello")
  (+ 2 2))
(block nil
  (return 0)
  (print "hello")
  (+ 2 2))

;;; P.16
(progn
  (print "hello")
  (+ 2 2))

;;; P.17
(if nil
    (print "hello")
    (print "world"))

(if (+ 2 2)
    (progn (print "hello")
           4)
    (print "world"))

(when (+ 2 2)
  (print "hello")
  4)
(unless (+ 2 2)
  (print "hello")
  4)
(unless nil
  (print "hello")
  4)

;;; P.18
(cond
  ((typep 4 'string)
   (print "hello"))
  ((> 4 2)
   (print "world")
   nil)
  (t
   (print "can't get here")))

;;; P.19
(dotimes (i 3)
  (print i))
(do ((i 0 (1+ i))
     (prompt (read-line) (read-line)))
    ((> i 1) i)
  (print (pair i prompt))
  (print "test"))

;;; P.20
;;; USE RUTILS
;(do ((i 0 (1+ i))
;     (prompt (read-line) (read-line)))
;    ((> i 1) i)
;  (print (pair i prompt))
;  (terpri))

;;; P.20
((lambda (x y) (+ x y)) 2 2)
(defun add2 (x y) (+ x y))
(add2 2 3)
(let ((x 2))
  (+ x x))
((lambda (x) (+ x x))
 2)
(let ((x 2)
      (y 2))
  (+ x y))

(let ((x 2))
  (print (+ x x))
  (setf x 4)
  (+ x x))

(let ((x 2))
  (print (+ x x))
  (let ((x 4))
    (print (+ x x)))
  (print (+ x x)))

(defparameter *temp* 1)
(progn
  (print *temp*)
  (let ((*temp* 2))
    (print *temp*)
    (setf *temp* 4)
    (print *temp*))
  *temp*)

(funcall 'add2 2 3)
(let ((add2 (lambda (x y) (+ x y))))
  (funcall add2 2 2))
(flet ((foo (x) (1+ x)))
  (foo 1))
(defun this ()
  "This has a curious docstring"
  (print "this"))
