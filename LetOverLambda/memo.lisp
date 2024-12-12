;;; Chapter 3
(defmacro nif (expr pos zero neg)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g) ,pos)
             ((zerop ,g) ,zero)
             (t ,neg)))))
(print (macroexpand '(nif x 'pos 'zero 'neg)))
"(LET ((#:G646 X))
  (COND ((PLUSP #:G646) 'POS) ((ZEROP #:G646) 'ZERO) (T 'NEG)))"

(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "G!"
                :start1 0
                :end1 2)))
(testing "g!-symbol-p"
  (ng (g!-symbol-p 's))
  (ng (g!-symbol-p "s"))
  (ng (g!-symbol-p 'testsymbol))
  (ok (g!-symbol-p 'G!test)))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (lol:flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defun o!-symbol-to-g!-symbol (s)
  (symb "G!"
        (subseq (symbol-name s) 2)))

(defun o!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "O!"
                :start1 0
                :end1 2)))

(defmacro square (x)
  (once-only (x)
    `(* ,x ,x)))
(defun square (x) (* x x))
(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))
;;;(defmacro! square (o!x)
;;;  `(* ,g!x ,g!x))
(macroexpand '(square (incf x)))

;;; Chapter 4
'(football-game
  (game-started-at
   #.(get-internal-real-time))
  (coin-flip
   #.(if (zerop (random 2)) 'heads 'tails)))
(progn
  (describe 'print)
  (describe 'setf)
  (describe '(hello))
  (defvar a "")
  (describe 'a)
  (describe :test)
  (describe 'lambda)
  (describe '(lambda (x) (print 'x))))

(defmacro! defunits% (quantity base-unit &rest units)
  `(defmacro ,(lol:symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un ((,base-unit) 1)
                ,@(mapcar (lambda (x)
                            `((,(car x)) ,(cadr x)))
                   (lol:group units 2))))))
(defun defunits-chaining% (u units)
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "Unknown unit ~a" u)
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain)
                 (defunits-chaining%
                     (cadr chain)
                   units))
              chain)))))
(defmacro! defunits%% (quantity base-unit &rest units)
  `(defmacro ,(symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un ((,base-unit) 1)
                ,@(mapcar
                   (lambda (x)
                     `((,(car x))
                       ,(defunits-chaining%
                            (car x)
                            (cons `(,base-unit 1)
                                  (group units 2)))))
                   (group units 2))))))

(defun defunits-chaining (u units prev)
  (if (member u prev)
      (error "~{ ~a~^ depends on~}" (cons u prev)))
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "Unknown unit ~a" u)
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain)
                 (defunits-chaining
                     (cadr chain)
                   units
                   (cons u prev)))
              chain)))))
(defmacro! defunits (quantity base-unit &rest units)
  `(defmacro ,(lol:symb 'unit-of- quantity) (,g!val ,g!un)
     `(* ,,g!val
         ,(case ,g!un ((,base-unit) 1)
                ,@(mapcar (lambda (x)
                            `((,(car x))
                              ,(defunits-chaining (car x)
                                   (cons
                                    `(,base-unit 1)
                                    (lol:group units 2))
                                 nil)))
                   (lol:group units 2))))))

;;;(defunits time s
;;;  m (1/60 h)
;;;  h (60 m))
;;; P.102
(defunits distance m
  km 1000
  cm 1/100
  mm (1/10 cm)
  nm (1/1000 mm)

  yard 9144/10000 ; Defined in 1956
  foot (1/3 yard)
  inch (1/12 foot)
  mile (1760 yard)
  furlong (1/8 mile)

  fathom (2 yard) ; Defined in 1929
  nautical-mile 1852

  cable (1/10 nautical-mile)
  old-brit-nautical-mile (6080/3 yard) ; Dropped in 1970
  old-brit-cable (1/10 old-brit-nautical-mile)
  old-brit-fathom (1/100 old-brit-cable))
;;; P.103
(ok
 (coerce (unit-of-distance 1/76 old-brit-fathom) 'float)
 0.024384)
;;; P.104
(defun tree-leaves% (tree result)
  (if tree
      (if (listp tree)
          (cons
           (tree-leaves% (car tree) result)
           (tree-leaves% (cdr tree) result))
          result)))
(ok
 (tree-leaves%
  '(2 (nil t (a . b)))
  'leaf)
 '(LEAF (NIL LEAF (LEAF . LEAF))))

;;; P.105
(ok
 (sort '(5 1 2 4 3 8 9 6 7) #'<)
 '(1 2 3 4 5 6 7 8 9))

(defun predicate-splitter (orderp splitp)
  (lambda (a b)
    (let ((s (funcall splitp a)))
      (if (eq s (funcall splitp b))
          (funcall orderp a b)
          s))))
(ok
 (sort '(5 1 2 4 3 8 9 6 7)
       (predicate-splitter #'< #'evenp))
 '(2 4 6 8 1 3 5 7 9))

;;; P.106
(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
          (cons
           (tree-leaves%% (car tree) test result)
           (tree-leaves%% (cdr tree) test result))
          (if (funcall test tree)
              (funcall result tree)
              tree))))

(ok
 (tree-leaves%%
  '(1 2 (3 4 (5 6)))
  (lambda (x) (and (numberp x) (evenp x)))
  (lambda (x) (declare (ignorable x)) 'even-number)))

;;; P.108
(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
    ,tree
    (lambda (x) (declare (ignorable x))
      ,test)
    (lambda (x) (declare (ignorable x))
      ,result)))

(ok
 (tree-leaves
  '(1 2 (3 4 (5 6)))
  (and (numberp x) (evenp x))
  'even-number)
 '(1 EVEN-NUMBER (3 EVEN-NUMBER (5 EVEN-NUMBER))))

