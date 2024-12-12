"https://gigamonkeys.com/book/"
"1. Introduction: Why Lisp?"
"https://gigamonkeys.com/book/introduction-why-lisp.html"

"2. Lather, Rinse, Repeat: A Tour of the REPL"
"https://gigamonkeys.com/book/lather-rinse-repeat-a-tour-of-the-repl.html"
(format t "~&hello, world")
(defun hello-world () (format t "hello, world~%"))
(hello-world)

"3. Practical: A Simple Database"
"https://gigamonkeys.com/book/practical-a-simple-database.html"
(list 1 2 3)
"property list, or plist for short"
(list :a 1 :b 2 :c 3)
(getf (list :a 1 :b 2 :c 3) :a)
(getf (list :a 1 :b 2 :c 3) :c)
(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))
(make-cd "Roses" "Kathy Mattea" 7 t)
(defvar *db* nil)
(defun add-record (cd) (push cd *db*))
(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))
(dump-db)
(format t "~a" "Dixie Chicks")
(format t "~a" :title)
(format t "~a:~10t~a" :artist "Dixie Chicks")
"one-liner for dump-db"
(defun dump-db ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(setf *db* (list
            (list :TITLE "Lyle Lovett" :ARTIST "Lyle Lovett" :RATING 9 :RIPPED T)
            (list :TITLE "Give Us a Break" :ARTIST "Limpopo" :RATING 10 :RIPPED T)
            (list :TITLE "Ben Folds" :ARTIST "6" :RATING 0 :RIPPED T)
            (list :TITLE "Home" :ARTIST "Dixie Chicks" :RATING 9 :RIPPED T)
            (list :TITLE "Fly" :ARTIST "Dixie Chicks" :RATING 8 :RIPPED T)
            (list :TITLE "Roses" :ARTIST "Kathy Mattea" :RATING 7 :RIPPED T))
      )
(save-db "~/practical_common_lisp.chap03.my-cds.tmp.db")
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'(lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'(lambda (x) (= 1 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'oddp '(1 2 3 4 5 6 7 8 9 10))

(remove-if-not
  #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)
(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))
(defun select (selector-fn)
  (remove-if-not selector-fn *db*))
(select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))
(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))
(select (artist-selector "Dixie Chicks"))

(defun foo (&key a b c) (list a b c))
(foo :a 1 :b 2 :c 3)
(foo :c 3 :b 2 :a 1)
(foo :a 1 :c 3)
(foo)

(defun foo (&key a (b 20) (c 30 c-p)) (list a b c c-p))
(foo :a 1 :b 2 :c 3)
(foo :c 3 :b 2 :a 1)
(foo :a 1 :c 3)
(foo)

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))
(select (where :artist "Dixie Chicks"))
(select (where :rating 10 :ripped nil))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(update (where :artist "Dixie Chicks") :rating 11)
(select (where :artist "Dixie Chicks"))
(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(select (where :title "Give Us a Break" :ripped t))
(select
 #'(lambda (cd)
     (and (equal (getf cd :title) "Give Us a Break")
          (equal (getf cd :ripped) t))))

(defmacro backwards (expr) (reverse expr))
(backwards ("hello, world" t format))

(defun make-comparison-expr (field value)
  (list 'equal (list 'getf 'cd field) value))
(make-comparison-expr :rating 10)
(make-comparison-expr :title "Give Us a Break")

`(1 2 (+ 1 2))
`(1 2 ,(+ 1 2))

(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
     collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))

`(and ,(list 1 2 3))
`(and ,@(list 1 2 3))
`(and ,@(list 1 2 3) 4)

(where :title "Give Us a Break" :ripped t)
(macroexpand-1 '(where :title "Give Us a Break" :ripped t))
(select (where :title "Give Us a Break" :ripped t))

"4 Syntax and Semantics"
"https://gigamonkeys.com/book/syntax-and-semantics.html"
"foo"
"fo\o"
"fo\\o"
"fo\"o"

(defun hello-world ()
  (format t "hello, world"))
(+ 1 2)
(* (+ 1 2) (- 3 4))
(quote (+ 1 2))
'(+ 1 2)
(let ((x 10)) x)

"EQ, EQL, EQUAL, and EQUALP"
"EQ tests for object identity:
you should never use EQ to compare values that may be numbers or characters."
"EQL to behave like EQ except that it also is guaranteed
to consider two objects of the same class representing the same numeric or
character value to be equivalent."

"The code in this book is written in the always use EQL style."

"EQUAL loosens the discrimination of EQL to consider lists equivalent if they have the
same structure and contents, recursively, according to EQUAL."
"EQUALP is similar to EQUAL except it's even less discriminating. It considers two
strings equivalent if they contain the same characters, ignoring differences in case."

"
Another important formatting rule is that closing parentheses are always put on the
same line as the last element of the list they're closing. That is, don't write this:

;;;; Four semicolons are used for a file header comment.
;;; A comment with three semicolons will usually be a paragraph
;;; comment that applies to a large section of code that follows,

(defun foo (x)
  (dotimes (i x)
    ;; Two semicolons indicate this comment applies to the code
    ;; that follows. Note that this comment is indented the same
    ;; as the code that follows.
    (some-function-call)
    (another i)              ; this comment applies to this line only
    (and-another)            ; and this is for this line
    (baz)))
"

"5 Functions"
"https://gigamonkeys.com/book/functions.html"
(defun hello-world () (format t "hello, world"))
(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))
(verbose-sum 1 2)

"optional parameters: &optional"
(defun foo (a b &optional c d) (list a b c d))
(foo 1 2)
(foo 1 2 3)
(foo 1 2 3 4)

(defun foo (a &optional (b 10)) (list a b))
(foo 1 2)
(foo 1)

(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))
(foo 1 2)
(foo 1 2 3)
(foo 1 2 4)

(format t "hello, world")
(format t "hello, ~a" '(1 2 3))
(format t "x: ~d y: ~d" 20.1 3)
(+)
(+ 1)
(+ 1 2)
(+ 1 2 3)

(defun foo (&key a b c) (list a b c))
(foo)                                   ;=> (NIL NIL NIL)
(foo :a 1)                              ;=> (1 NIL NIL)
(foo :b 1)                              ;=> (NIL 1 NIL)
(foo :c 1)                              ;=> (NIL NIL 1)
(foo :a 1 :c 3)                         ;=> (1 NIL 3)
(foo :a 1 :b 2 :c 3)                    ;=> (1 2 3)
(foo :a 1 :c 3 :b 2)                    ;=> (1 2 3)

(defun foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(foo :a 1)                              ;=> (1 0 1 NIL)
(foo :b 1)                              ;=> (0 1 1 T)
(foo :b 1 :c 4)                         ;=> (0 1 4 T)
(foo :a 2 :b 1 :c 4)                    ;=> (2 1 4 T)

(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c) 0 c-supplied-p))
  (list a b c c-supplied-p))
(foo :apple 10 :box 20 :charlie 30)     ;=> (10 20 30 T)

(defun foo (x &optional y &key z) (list x y z))
"
; caught STYLE-WARNING:
;   &OPTIONAL and &KEY found in the same lambda list: (X &OPTIONAL Y &KEY Z)
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition
"

(foo 1 2 :z 3)                          ;=> (1 2 3)
(foo 1)                                 ;=> (1 nil nil)
;(foo 1 :z 3)                            ;=> ERROR

(defun foo (&rest rest &key a b c) (list rest a b c))
(foo :a 1 :b 2 :c 3)                    ;=> ((:A 1 :B 2 :C 3) 1 2 3)

(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))
(foo 3)

(defun foo (x) (* x x))
(mapcar #'foo '(1 2 3 4))

(defun foo (a b c) (list a b c))
(foo 1 2 3)
(funcall #'foo 1 2 3)

(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))
(plot (lambda (x) (* x x)) -4 4 1)
(plot #'exp 0 4 1/2)
(apply #'append '((1 2) (3 4)))
(apply #'plot '(exp 0 4 1/2))
(apply #'plot '(exp 0 4 1/2))

(funcall #'(lambda (x y) (+ x y)) 2 3)  ;=> 5
((lambda (x y) (+ x y)) 2 3)            ;=> 5

(defun mult-by-2 (x) (* 2 x))
(plot #'mult-by-2 0 10 1)
(plot #'(lambda (x) (* 2 x)) 0 10 1)

"6. Variables"
"https://gigamonkeys.com/book/variables.html"
(let ((a 1) (b 2))
  (+ a b))                              ; => 3
(defun foo (x)
  (format t "Parameter: ~a~%" x)      ; |<------ x is argument
  (let ((x 2))                        ; |
    (format t "Outer LET: ~a~%" x)    ; | |<---- x is 2
    (let ((x 3))                      ; | |
      (format t "Inner LET: ~a~%" x)) ; | | |<-- x is 3
    (format t "Outer LET: ~a~%" x))   ; | |
  (format t "Parameter: ~a~%" x))     ; |
(foo 1)

(dotimes (x 10) (format t "~d " x))

(let* ((x 10)
       (y (+ x 10)))
  (list x y))
(let ((x 10))
  (let ((y (+ x 10)))
    (list x y)))

(let ((count 0)) #'(lambda () (setf count (1+ count))))
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
(funcall *fn*)
(funcall *fn*)
(funcall *fn*)
(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count)))

"基本は defvar を使おう"
(defvar *count* 0
  "Count of widgets made so far.")
(defparameter *gap-tolerance* 0.001
  "Tolerance to be allowed in widget gaps.")
;(defun increment-widget-count () (incf *count*))
;(increment-widget-count)
(defvar *x* 10)
(defun foo () (format t "X: ~d~%" *x*))
(foo)
(let ((*x* 20)) (foo))
(defun bar ()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))
(bar)
(defun foo ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))
(foo)
(bar)

(defvar *x1* 10)
(defun foo1 () (format t "X1: ~d~%" *x1*))
(foo1)
(let ((*x1* 20)) (foo1))
(defun bar1 ()
  (foo1)
  (let ((*x1* 20)) (foo1))
  (foo1))
(bar1)

(defvar x 1)
(setf x 10)
(defun foo (x) (setf x 100))
(foo 10)
(let ((y 20))
  (foo y)
  (print y))

"7 Macros: Standard Control Constructs"
"https://gigamonkeys.com/book/macros-standard-control-constructs.html"

(if (> 2 3) "Yup" "Nope")               ;==> "Nope"
(if (> 2 3) "Yup")                      ;==> NIL
(if (> 3 2) "Yup" "Nope")               ;==> "Yup"

(not nil)             ;==> T
(not (= 1 1))         ;==> NIL
(and (= 1 2) (= 3 3)) ;==> NIL
(or (= 1 2) (= 3 3))  ;==> T

(dolist (x '(1 2 3)) (print x))
(dolist (x '(1 2 3)) (print x) (if (evenp x) (return)))
(dotimes (i 4) (print i))

(dotimes (x 20)
  (dotimes (y 20)
    (format t "~3d " (* (1+ x) (1+ y))))
  (format t "~%"))

(do ((n 0 (1+ n))
     (cur 0 next)
     (next 1 (+ cur next)))
    ((= 10 n) cur))
(do ((i 0 (1+ i)))
    ((>= i 4))
  (print i))

(do ((nums nil) (i 1 (1+ i)))
    ((> i 10) (nreverse nums))
  (push i nums))                        ;==> (1 2 3 4 5 6 7 8 9 10)
(loop for i from 1 to 10 collecting i)  ;==> (1 2 3 4 5 6 7 8 9 10)
(loop for x from 1 to 10 summing (expt x 2)) ;==> 385

"This counts the number of vowels in a string:"
(loop for x across "the quick brown fox jumps over the lazy dog"
      counting (find x "aeiou"))        ; ==> 11

"This computes the eleventh Fibonacci number,
similar to the DO loop used earlier:"
(loop for i below 10
      and a = 0 then b
      and b = 1 then (+ b a)
      finally (return  a))

"8 Macros: Defining Your Own"
"https://gigamonkeys.com/book/macros-defining-your-own.html"
(defun foo (x)
  (when (> x 10) (print 'big)))
(mapcar #'foo '(1 100))

(defmacro my-when (condition &rest body)
  `(if ,condition (progn ,@body)))
(macroexpand '(my-when 1))

(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))
(defun next-prime (number)
  (loop for n from number when (primep n) return n))

(defmacro do-primes (var-and-range &rest body)
  (let ((var (first var-and-range))
        (start (second var-and-range))
        (end (third var-and-range)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))
(do-primes (p 0 19) (format t "~d " p))
(time (do-primes (p 0 20000) (format t "~d " p)))
(macroexpand-1 '(do-primes (p 0 19) (format t "~d " p)))

"fix the multiple evaluation"
(defmacro do-primes ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

"fix a leak"
(defmacro do-primes ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))

"gensym"
(defmacro do-primes ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(do-primes (ending-value 0 10)
  (print ending-value))

(loop for n in '(a b c) collect `(,n (gensym)))
(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))
(defmacro do-primes ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))
(defmacro do-primes ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))

"9 Practical: Building a Unit Test Framework"
"https://gigamonkeys.com/book/practical-building-a-unit-test-framework.html"
(defun test-+ ()
  (and
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
(test-+)

(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
(test-+)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+ ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))
(test-+)

(defmacro check (form)
  `(report-result ,form ',form))
(defun test-+ ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))
(test-+)

(defmacro check (&body forms)
  `(progn
     ,@(loop for f in forms collect `(report-result ,f ',f))))

(defun test-+ ()
  (check
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))
(test-+)

(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
      ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))
(defmacro check (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result ,f ',f))))
(test-+)

(defun test-* ()
  (check
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(defun test-arithmetic ()
  (combine-results
   (test-+)
   (test-*)))

"Wrapping Up"
(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
       ,result)))

(defun report-result (result form)
  "Report the results of a single test case. Called by 'check'."
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

"10 Numbers, Characters, and Strings"
"https://gigamonkeys.com/book/numbers-characters-and-strings.html"
10
20/2
#xa
#b10101
#b1010/1011
#o777
#xDADA
#36rABCDEFGHIJKLMNOPQRSTUVWXYZ
1.0
1e0
1d0
123.0
123e0
0.123
.123
123e-3
123E-3
0.123e20
123d23

"Complex numbers"
(+ #c(2      1))
(+ #c(2/3  3/4))
(+ #c(2    1.0))
(+ #c(2.0  1.0d0))
(+ #c(1/2  1.0))
(+ #c(3      0))
(+ #c(3.0  0.0))
(+ #c(1/2    0))
(+ #c(-6/3   0))
(+ #c(1 2) #c(3 4))

(+ 1 2.0)
(/ 2 3.0)
(+ #c(1 2) 3)
(+ #c(1 2) 3/2)
(+ #c(1 1) #c(2 -1))

(= 1 1)
(= 10 20/2)
(= 1 1.0 #c(1.0 0.0) #c(1 0))

(/= 1 1)
(/= 1 2)
(/= 1 2 3)
(/= 1 2 3 1)
(/= 1 2 3 1.0)

(< 2 3)
(> 2 3)
(> 3 2)
(< 2 3 4)
(< 2 3 3)
(<= 2 3 3)
(<= 2 3 3 4)
(<= 2 3 4 3)

(max 10 11)
(min -12 -10)
(max -1 2 -3)

(string/= "lisp" "lissome")
(string< "lisp" "lisper")
(string< "foobar" "abaz" :start1 3 :start2 1)

"11 Collections"
"https://gigamonkeys.com/book/collections.html"

"Vectors"
(vector)
(vector 1)
(vector 1 2)
(make-array 5 :initial-element nil)
(make-array 5 :initial-element 1)
(make-array 5 :fill-pointer 0)

(defparameter *x* (make-array 5 :fill-pointer 0))
(vector-push 'a *x*)
*x*
(vector-push 'b *x*)
*x*
(vector-push 'c *x*)
*x*
(vector-pop *x*)
*x*
(vector-pop *x*)
*x*
(vector-pop *x*)
*x*

(make-array 5 :fill-pointer 0 :adjustable t)
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)

(defparameter *x* (vector 1 2 3))
(length *x*)
(elt *x* 0)
(elt *x* 1)
(elt *x* 2)
;(elt *x* 3)
(setf (elt *x* 0) 10)
*x*

"Table 11-1.Basic Sequence Functions"
(count 1 #(1 2 1 2 3 1 2 3 4))
(remove 1 #(1 2 1 2 3 1 2 3 4))
(remove 1 '(1 2 1 2 3 1 2 3 4))
(remove #\a "foobarbaz")
(substitute 10 1 #(1 2 1 2 3 1 2 3 4))
(substitute 10 1 '(1 2 1 2 3 1 2 3 4))
(substitute #\x #\b "foobarbaz")
(find 1 #(1 2 1 2 3 1 2 3 4))
(find 10 #(1 2 1 2 3 1 2 3 4))
(position 1 #(1 2 1 2 3 1 2 3 4))

(count "foo" #("foo" "bar" "baz") :test #'string=)
(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'first)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first)
(find 'a #((a 10) (b 20) (a 30) (b 40)) :key #'first :from-end t)
(remove #\a "foobarbaz" :count 1)
(remove #\a "foobarbaz" :count 1 :from-end t)

(defparameter *v* #((a 10) (b 20) (a 30) (b 40)))
(defun verbose-first (x) (format t "Looking at ~s~%" x) (first x))
(count 'a *v* :key #'verbose-first)
(count 'a *v* :key #'verbose-first :from-end t)

"Higher-Order Function Variants"
(count-if #'evenp #(1 2 3 4 5))
(count-if-not #'evenp #(1 2 3 4 5))
(position-if #'digit-char-p "abcd0001")
(remove-if-not #'(lambda (x) (char= (elt x 0) #\f))
               #("foo" "bar" "baz" "foom"))
(count-if #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first)
(count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'first)
(remove-if-not #'alpha-char-p
               #("foo" "bar" "1baz") :key #'(lambda (x) (elt x 0)))
(remove-duplicates #(1 2 1 2 3 1 2 3 4))

"Whole Sequence Manipulations"
(concatenate 'vector #(1 2 3) '(4 5 6))
(concatenate 'list #(1 2 3) '(4 5 6))
(concatenate 'string "abc" '(#\d #\e #\f))
(sort (vector "foo" "bar" "baz") #'string<)

(let ((my-sequence '()))
  (setf my-sequence (sort my-sequence #'string<)))

(merge 'vector #(1 3 5) #(2 4 6) #'<)
(merge 'list #(1 3 5) #(2 4 6) #'<)

(subseq "foobarbaz" 3)
(subseq "foobarbaz" 3 6)

(defparameter *x* (copy-seq "foobarbaz"))
(setf (subseq *x* 3 6) "xxx")  ; subsequence and new value are same length
*x*
(setf (subseq *x* 3 6) "abcd") ; new value too long, extra character ignored.
*x*
(setf (subseq *x* 3 6) "xx")   ; new value too short, only two characters changed
*x*

(position #\b "foobarbaz")
(search "bar" "foobarbaz")
(mismatch "foobarbaz" "foom")

(mismatch "foobar" "bar" :from-end t)

(every #'evenp #(1 2 3 4 5))
(some #'evenp #(1 2 3 4 5))
(notany #'evenp #(1 2 3 4 5))
(notevery #'evenp #(1 2 3 4 5))

(every #'> #(1 2 3 4) #(5 4 3 2))
(map 'vector #'* #(1 2 3 4 5) #(10 9 8 7 6))
(setq a (list 1 2 3 4) b (list 10 10 10 10))
(map-into a #'+ a b)
(reduce #'+ #(1 2 3 4 5 6 7 8 9 10))

"Hash Tables"
(defparameter *h* (make-hash-table))
(gethash 'foo *h*)
(setf (gethash 'foo *h*) 'quux)
(gethash 'foo *h*)

(defun show-value (key hash-table)
  (multiple-value-bind (value present) (gethash key hash-table)
    (if present
      (format nil "Value ~a actually present." value)
      (format nil "Value ~a because key not found." value))))
(setf (gethash 'bar *h*) nil) ; provide an explicit value of NIL
(show-value 'foo *h*) ;==> "Value QUUX actually present."
(show-value 'bar *h*) ;==> "Value NIL actually present."
(show-value 'baz *h*) ;==> "Value NIL because key not found."

"Hash Table Iteration"
(maphash #'(lambda (k v) (format t "~a => ~a~%" k v)) *h*)
                                        ;(maphash #'(lambda (k v) (when (< v 10) (remhash k *h*))) *h*)
(loop for k being the hash-keys in *h* using (hash-value v)
  do (format t "~a => ~a~%" k v))

"12 They Called It LISP for a Reason: List Processing"
"https://gigamonkeys.com/book/they-called-it-lisp-for-a-reason-list-processing.html"

"There Is No List"
(cons 1 2)
(cons 1 '(2))
(car (cons 1 2))
(cdr (cons 1 2))

(defparameter *cons* (cons 1 2))
*cons*                 ;==> (1 . 2)
(setf (car *cons*) 10) ;==> 10
*cons*                 ;==> (10 . 2)
(setf (cdr *cons*) 20) ;==> 20
*cons*                 ;==> (10 . 20)

(cons 1 nil)                   ;==> (1)
(cons 1 (cons 2 nil))          ;==> (1 2)
(cons 1 (cons 2 (cons 3 nil))) ;==> (1 2 3)

(list 1)     ;==> (1)
(list 1 2)   ;==> (1 2)
(list 1 2 3) ;==> (1 2 3)

(defparameter *list* (list 1 2 3 4))
(first *list*)        ;==> 1
(rest *list*)         ;==> (2 3 4)
(first (rest *list*)) ;==> 2

(append (list 1 2) (list 3 4))

"\"Destructive\" Operations"
(defparameter *x* (list 1 2 3))
(nconc *x* (list 4 5 6))                ;==> (1 2 3 4 5 6)
*x*                                     ;==> (1 2 3 4 5 6)

(defun upto (max)
  (let ((result nil))
    (dotimes (i max)
      (push i result))
    (nreverse result)))
(upto 10)                               ;==> (0 1 2 3 4 5 6 7 8 9)

(defparameter *list* (list 4 3 2 1))
(sort *list* #'<)
*list*

"13 Beyond Lists: Other Uses for Cons Cells"
"https://gigamonkeys.com/book/beyond-lists-other-uses-for-cons-cells.html"
(defparameter *set* ())
(adjoin 1 *set*)
*set*
(setf *set* (adjoin 1 *set*))
(pushnew 2 *set*)
*set*
(pushnew 2 *set*)

(subsetp '(3 2 1) '(1 2 3 4))
(subsetp '(1 2 3 4) '(3 2 1))

(assoc 'a '((a . 1) (b . 2) (c . 3)))
(assoc 'c '((a . 1) (b . 2) (c . 3)))
(assoc 'd '((a . 1) (b . 2) (c . 3)))
(cdr (assoc 'a '((a . 1) (b . 2) (c . 3))))
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)) :test #'string=)
(assoc "a" '(("a" . 1) ("b" . 2) ("c" . 3)))
(assoc 'a '((a . 10) (a . 1) (b . 2) (c . 3)))
(pairlis '(a b c) '(1 2 3))

(defparameter *plist* ())
*plist*
(setf (getf *plist* :a) 1)
*plist*
(setf (getf *plist* :a) 2)
*plist*
(remf *plist* :a)

(destructuring-bind (x y z) (list 1 2 3)
  (list :x x :y y :z z)) ;==> (:X 1 :Y 2 :Z 3)
(destructuring-bind (x y z) (list 1 (list 2 20) 3)
  (list :x x :y y :z z)) ;==> (:X 1 :Y (2 20) :Z 3)
(destructuring-bind (x (y1 y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ;==> (:X 1 :Y1 2 :Y2 20 :Z 3)
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2 20) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ;==> (:X 1 :Y1 2 :Y2 20 :Z 3)
(destructuring-bind (x (y1 &optional y2) z) (list 1 (list 2) 3)
  (list :x x :y1 y1 :y2 y2 :z z)) ;==> (:X 1 :Y1 2 :Y2 NIL :Z 3)
(destructuring-bind (&key x y z) (list :x 1 :y 2 :z 3)
  (list :x x :y y :z z)) ;==> (:X 1 :Y 2 :Z 3)
(destructuring-bind (&key x y z) (list :z 1 :y 2 :x 3)
  (list :x x :y y :z z)) ;==> (:X 3 :Y 2 :Z 1)

"14 Files and File I/O"
"https://gigamonkeys.com/book/files-and-file-io.html"
(map nil
     #'print
     (directory "/home/cl/*.*")) ; slime-dockerで動かしている
(truename "/home/cl")
(sb-posix:getcwd)
(probe-file #P"/home/cl/sample.lisp")
(let ((in (open "/home/cl/sample.lisp" :if-does-not-exist nil)))
  (when in
    (format t "~a~%" (read-line in))
    (close in)))

(let ((in (open "/home/cl/sample.lisp" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (format t "~a~%" line))
    (close in)))

(let ((in (open "/home/cl/read.sample.lisp" :if-does-not-exist nil)))
  (when in
    (format t "~&~a" (read in))
    (format t "~&~a" (read in))
    (format t "~&~a" (read in))
    (format t "~&~a" (read in))
    (close in)))

(with-open-file (stream "/home/cl/read.sample.lisp")
  (format t "~a~%" (read-line stream)))
(with-open-file (stream "/home/cl/practical_common_lisp_chap14.write.tmp.txt" :direction :output)
  (format stream "Some text."))

(pathname-directory (pathname "/foo/bar/baz.txt")) ;==> (:ABSOLUTE "foo" "bar")
(pathname-name (pathname "/foo/bar/baz.txt"))      ;==> "baz"
(pathname-type (pathname "/foo/bar/baz.txt"))      ;==> "txt"
(pathname "/foo/bar/baz.txt")
(namestring #p"/foo/bar/baz.txt")           ;==> "/foo/bar/baz.txt"
(directory-namestring #p"/foo/bar/baz.txt") ;==> "/foo/bar/"
(file-namestring #p"/foo/bar/baz.txt")      ;==> "baz.txt"

(make-pathname
  :directory '(:absolute "foo" "bar")
  :name "baz"
  :type "txt") ;==> #p"/foo/bar/baz.txt"
(make-pathname
 :directory '(:relative "backups")
 :defaults #p"/foo/bar/baz.txt") ;==> #p"backups/baz.txt"

(merge-pathnames #p"foo/bar.html" #p"/www/html/")
(merge-pathnames #p"foo/bar.html" #p"html/")
(enough-namestring #p"/www/html/foo/bar.html" #p"/www/")
(merge-pathnames
 (enough-namestring #p"/www/html/foo/bar/baz.html" #p"/www/")
 #p"/www-backups/") ;==> #p"/www-backups/html/foo/bar/baz.html"
(make-pathname :name "foo" :type "txt") ;==> #p"foo.txt"
(merge-pathnames #p"foo.txt")

"15 Practical: A Portable Pathname Library"
"https://gigamonkeys.com/book/practical-a-portable-pathname-library.html"

"16 Object Reorientation: Generic Functions"
"https://gigamonkeys.com/book/object-reorientation-generic-functions.html"

"17 Object Reorientation: Classes"
"https://gigamonkeys.com/book/object-reorientation-classes.html"

(defclass bank-account ()
  (customer-name
   balance))
(make-instance 'bank-account)
(defparameter *account* (make-instance 'bank-account))  ;==> *ACCOUNT*
(setf (slot-value *account* 'customer-name) "John Doe") ;==> "John Doe"
(setf (slot-value *account* 'balance) 1000)             ;==> 1000
(slot-value *account* 'customer-name) ;==> "John Doe"
(slot-value *account* 'balance)       ;==> 1000

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name)
   (balance
    :initarg :balance
    :initform 0)))
(defparameter *account*
  (make-instance 'bank-account :customer-name "John Doe" :balance 1000))
(slot-value *account* 'customer-name) ;==> "John Doe"
(slot-value *account* 'balance)       ;==> 1000

(defvar *account-numbers* 0)
(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))))

(defclass bank-account ()
  ((customer-name
    :initarg :customer-name
    :initform (error "Must supply a customer name."))
   (balance
    :initarg :balance
    :initform 0)
   (account-number
    :initform (incf *account-numbers*))
   account-type))
(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))

(defmethod initialize-instance :after ((account bank-account) &key)
  (let ((balance (slot-value account 'balance)))
    (setf (slot-value account 'account-type)
          (cond
            ((>= balance 100000) :gold)
            ((>= balance 50000) :silver)
            (t :bronze)))))

"18 A Few FORMAT Recipes"
"https://gigamonkeys.com/book/a-few-format-recipes.html"

(let ((lst '(:a :b :c :d)))
  (loop for cons on lst
        do (format t "~a" (car cons))
        when (cdr cons) do (format t ", "))
  (format t "~&~{~a~^, ~}" lst))


"19 Beyond Exception Handling: Conditions and Restarts"
"https://gigamonkeys.com/book/beyond-exception-handling-conditions-and-restarts.html"

"20 The Special Operators"
"https://gigamonkeys.com/book/the-special-operators.html"

"21 Programming in the Large: Packages and Symbols"
"https://gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html"

"22 LOOP for Black Belts"
"https://gigamonkeys.com/book/loop-for-black-belts.html"

"23 Practical: A Spam Filter"
"https://gigamonkeys.com/book/practical-a-spam-filter.html"

"24 Practical: Parsing Binary Files"
"https://gigamonkeys.com/book/practical-parsing-binary-files.html"

"25 Practical: An ID3 Parser"
"https://gigamonkeys.com/book/practical-an-id3-parser.html"

"26 Practical: Web Programming with AllegroServe"
"https://gigamonkeys.com/book/practical-web-programming-with-allegroserve.html"

"27 Practical: An MP3 Database"
"https://gigamonkeys.com/book/practical-an-mp3-database.html"

"28 Practical: A Shoutcast Server"
"https://gigamonkeys.com/book/practical-a-shoutcast-server.html"

"29 Practical: An MP3 Browser"
"https://gigamonkeys.com/book/practical-an-mp3-browser.html"

"30 Practical: An HTML Generation Library, the Interpreter"
"https://gigamonkeys.com/book/practical-an-html-generation-library-the-interpreter.html"

"31 Practical: An HTML Generation Library, the Compiler"
"https://gigamonkeys.com/book/practical-an-html-generation-library-the-compiler.html"

"32 Conclusion: What's Next?"
"https://gigamonkeys.com/book/conclusion-whats-next.html"
