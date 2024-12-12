"P.164"
(cons 'rice '(and beans))
(list 'rice '(and beans))
;;;(append 'rice '(and beans))
(cons '(here today) '(gone tomorrow))
(list '(here today) '(gone tomorrow))
(append '(here today) '(gone tomorrow))
(cons '(eat at) 'joes)
(list '(eat at) 'joes)
(append '(eat at) 'joes)

(reverse '(one two three four five))
(reverse '(l i v e))
(reverse "LIVE")

(nthcdr 0 '(a b c))
(nthcdr 1 '(a b c))
(nthcdr 2 '(a b c))
(nthcdr 3 '(a b c))

(last '(all is forgiven))
(last nil)
(last '(a b c . d))
(last '(ROSEBUD))
(last '((A B C)))

(remove 'a '(b a n a n a))
(remove 1 '(3 1 4 1 5 9))

"P.171"
(setf ducks '(huey dewey louie))
(member 'huey ducks)
(member 'dewey ducks)
(member 'louie ducks)

"P.179 6.8 LISTS AS TABLES"
(setf words
      '((one un)
        (two deux)
        (three trois)
        (four quatre)
        (five cinq)))
(assoc 'three words)
(assoc 'four words)
(assoc 'six words)

"P.180 6.8.2 RASSOC: reverse assoc"
(setf sounds
      '((cow . moo)
        (pig . oink)
        (cat . meow)
        (dog . woof)
        (bird . tweet)))
(rassoc 'woof sounds)
(assoc 'dog sounds)

"P.196"
(let*
    ((x1 (list 'a 'b 'c))
     (x2 (list 'a 'b 'c))
     )
  (print (equal x1 x2))
  (print (eq x1 x2))
  )

(eql 'foo 'foo)
(eql 3 3)
(eql 3 3.0)
(= 3 3.0)

"P.198
- EQ is the fastest equality test: It compares addresses. Experts use
it to compare symbols quickly, and to test whether two cons cells
are physically the same object. It should not be used to compare
numbers.
- EQL is like EQ except it can safely compare numbers of the same
type, such as two integers or two floating point numbers. It is the
default equality test in Common Lisp.
- EQUAL is the predicate beginners should use. It compares lists
element by element; otherwise it works like EQL.
- EQUALP is more liberal than EQUAL: It ignores case distinctions
in strings, among other things.
- = is the most efficient way to compare numbers, and the only way
to compare numbers of disparate types, such as 3 and 3.0. It only
accepts numbers."

(symbolp :count)
(equal :count 'count)
(keywordp 'count)
(keywordp :count)

"P.201 7 Applicative Programming"
(funcall #'cons 'a 'b)

(setf fn #'cons)
fn
(type-of fn)
(funcall fn 'c 'd)
#'if

(defun square (n) (* n n))
(square 3)
(mapcar #'square '(1 2 3 4 5))
(mapcar #'square '(3 8 -3 5 2 10))

(let* ((words
         '((one un)
           (two deux)
           (three trois)
           (four quatre)
           (five cinq))))
  (print (mapcar #'first words))
  (print (mapcar #'second words))
  (print (mapcar #'reverse words))
  (defun translate (x)
    (second (assoc x words)))
  (print (mapcar #'translate '(three one four one five)))
  )

(defun add1 (x) (1+ x))
(mapcar #'add1 '(1 2 3 4 5))
(mapcar #'zerop '(2 0 3 4 0 -5 -6))
(mapcar #'(lambda (x) (> x 0)) '(2 0 3 4 0 -5 -6))

"P.205"
(mapcar #'(lambda (n) (* n n)) '(1 2 3 4 5))
(mapcar #'(lambda (n) (* n 10)) '(1 2 3 4 5))
(mapcar #'(lambda (x) (list 'hi 'there x))
        '(joe fred wanda))

"P.207"
(find-if #'oddp '(2 4 6 7 8 9))

"P.210"
(remove-if #'numberp '(2 for 1 sale))
(remove-if #'oddp '(1 2 3 4 5 6 7))
(remove-if #'(lambda (x) (not (plusp x)))
           '(2 0 -4 6 -8 10))
(remove-if-not #'plusp '(2 0 -4 6 -8 10))
(remove-if-not #'oddp '(2 0 -4 6 -8 10))
(remove-if-not #'(lambda (x) (> x 3))
               '(2 4 6 8 4 2 1))
(remove-if-not #'numberp
               '(3 apples 4 pears and 2 little plums))
(remove-if-not #'symbolp
               '(3 apples 4 pears and 2 little plums))

"P.213 reduce"
(reduce #'+ '(1 2 3))
(reduce #'+ '(10 9 8 7 6))
(reduce #'+ '(5))
(reduce #'+ nil)
(reduce #'* '(2 4 5))
(reduce #'* '(3 4 0 7))
(reduce #'* '(8))
(reduce #'append
        '((one un) (two deux) (three trois)))

"P.214"
(every #'numberp '(1 2 3 4 5))
(every #'numberp '(1 2 A B C 5))

(every #'(lambda (x) (> x 0)) '(1 2 3 4 5))
(every #'(lambda (x) (> x 0)) '(1 2 3 -4 5))

(every #'> '(10 20 30 40) '(1 5 11 23))

"P.216 trace"
(defun half (n) (* n 0.5))
(defun average (x y)
  (+ (half x) (half y)))
(trace half average)
(average 3 7)
(untrace)

"P.224"
(mapcar #'(lambda (x y) (list x 'gets y))
        '(fred wilma george diane)
        '(job1 job2 job3 job4))
(mapcar #'+ '(1 2 3 4 5) '(60 70 80 90 100))
(mapcar #'+ '(1 2 3) '(10 20 30 40 50))

#'(lambda (x) (+ x 2))

(let*
    ((g #'(lambda (x) (* x 10))))
  (funcall g 12))

(find-if #'oddp '(2 3 4 5 6))
(find-if #'oddp '(2 3 4 5 6) :from-end t)

(reduce #'cons '(a b c d e))
(reduce #'cons '(a b c d e) :from-end t)

"P.230"
(defun make-greater-than-predicate (n)
  #'(lambda (x) (> x n)))
(funcall (make-greater-than-predicate 3) 2)
(funcall (make-greater-than-predicate 3) 5)
(find-if (make-greater-than-predicate 3) '(2 3 4 5 6 7 8 9))

"P.231 8. Recursion"
(defun anyoddp (x)
  (cond ((null x) nil)
        ((oddp (first x)) t)
        (t (anyoddp (rest x)))))
(trace anyoddp)
(anyoddp nil)
(anyoddp '(7))
(anyoddp '(6 7))
(anyoddp '(2 4 6 7 8 9))
(untrace anyoddp)

(defun fact (n)
  (cond ((zerop n) 1)
        (t (* n (fact (- n 1))))))
(trace fact)
(fact 5)
(untrace fact)

(defun count-slices (loaf)
  (cond ((null loaf) 0)
        (t (+ 1 (count-slices (rest loaf))))))
(trace count-slices)
(count-slices nil)
(count-slices '(x))
(count-slices '(x x x x x))
(untrace count-slices)

(defun find-first-atom (x)
  (cond ((atom x) x)
        (t (find-first-atom (first x)))))

(defun my-nth (n x)
  (cond ((zerop n) (first x))
        (t (my-nth (- n 1) (rest x)))))
(my-nth 2 '(a b c d e))

(defun extract-symbols (x)
  (cond ((null x) nil)
        ((symbolp (first x))
         (cons (first x)
               (extract-symbols (rest x))))
        (t (extract-symbols (rest x)))))

(defun find-number (x)
  (cond ((numberp x) x)
        ((atom x) nil)
        (t (or (find-number (car x))
               (find-number (cdr x))))))

(defun tr-cs1 (loaf n)
  (cond ((null loaf) n)
        (t (tr-cs1 (rest loaf) (+ n 1)))))
(defun tr-count-slices (loaf)
  (tr-cs1 loaf 0))

(defun my-reverse (x)
  "not tail recursive"
  (cond ((null x) nil)
        (t (append (reverse (rest x))
                   (list (first x))))))

(defun tr-rev1 (x result)
  (cond ((null x) result)
        (t (tr-rev1
            (rest x)
            (cons (first x) result)))))
(defun tr-reverse (x)
  "tail recursive"
  (tr-rev1 x nil))

(defun count-up (n)
  (labels ((count-up-recursively (cnt)
             (if (> cnt n) nil
                 (cons cnt
                       (count-up-recursively
                        (+ cnt 1))))))
    (count-up-recursively 1)))
(count-up 1)
(count-up 2)
(count-up 3)
(count-up 4)

(- 0 1 2 3 4 5)

"P.287 9. Input/Output"
(stringp "strings are things")
(format t "Hi, mom!")
(format t "Time flies~%like an arrow.")
(format t "Fruit flies~%~%like bananas.")

(defun mary ()
  (format t "~&Mary had a little bat.")
  (format t "~&Its wings were long and brown.")
  (format t "~&And everywhere that Mary went")
  (format t "~&The bat went, upside-down."))
(mary)

(format t "From ~S to ~S in ~S minutes!"
        'boston '(new york) 55)

(defun square-talk (n)
  (format t "~&~S squared is ~S" n (* n n)))
(square-talk 10)
(mapcar #'square-talk '(1 2 3 4 5))

(defun test (x)
  (format t "~&With escape characters: ~S" x)
  (format t "~&Without escape characters: ~A" x))
(test "Hi, mom")

(defun my-square ()
  (format t "Please type in a number: ")
  (let ((x (read)))
    (format t "The number ~S squared is ~S.~%"
            x (* x x))))

(defun riddle ()
  (if (yes-or-no-p
       "Do you seek Zen enlightenment? ")
      (format t "Then do not ask for it!")
      (format t "You have found it.")))

(defun get-tree-data ()
  (with-open-file (stream "/home/cl/timber.dat")
    (let* ((tree-loc (read stream))
           (tree-table (read stream))
           (num-trees (read stream)))
      (format t "~&There are ~S trees on ~S."
              num-trees tree-loc)
      (format t "~&They are: ~S" tree-table))))
(get-tree-data)

(defun save-tree-data (tree-loc tree-table
                       num-trees)
  (with-open-file (stream "/home/cl/timber.newdat"
                          :direction :output)
    (format stream "~S~%" tree-loc)
    (format stream "~S~%" tree-table)
    (format stream "~S~%" num-trees)))
;(save-tree-data "The West Ridge"
; '((45 redwood) (22 oak) (43 maple))
; 110)
(dribble "/home/cl/session1.tmp.log")
(list 'a 'b)
(dribble)

(defun print-one-name (name)
  (format t "~&~10S ~S"
          (second name)
          (first name)))
(defun print-all-names (x)
  (mapcar #'print-one-name x)
  'done)
(let*
    ((glee-club '((john smith) (barbara wilson) (mustapha ali))))
  (print-all-names glee-club)
  )

(defun sevenths (x)
  (mapcar #'(lambda (numerator)
              (format t "~&~4,2F / 7 is ~7,5F"
                      numerator
                      (/ numerator 7.0)))
          x)
  'done)
(sevenths '(1 3/2 2 2.5 3))

"P.307 10. Assignment"
(defvar *total-glasses* 0)
(defun sell (n)
  "Ye Olde Lemonade Stand: Sales by the Glass."
  (setf *total-glasses* (+ *total-glasses* n))
  (format t
          "~&That makes ~S glasses so far today."
          *total-glasses*))
(sell 3)
(sell 2)

(defvar *a* 2)
(incf *a*)
(incf *a* 2)
(decf *a* 3)

(defun bad-style (n)
  (format t "~&N is ~S." n)
  (decf n 2)
  (format t "~&Now N is ~S." n)
  (decf n 2)
  (format t "~&Now N is ~S." n)
  (list 'result 'is (* n (- n 1))))
(bad-style 9)

(defun good-style (n)
  (let* ((p (- n 2))
         (q (- p 2)))
    (format t "~&N is ~S." n)
    (format t "~&P is ~S." p)
    (format t "~&Q is ~S." q)
    (list 'result 'is (* q (- q 1)))))
(good-style 9)

(defun picky-multiply (x y)
  "Computes X times Y. Input X must be odd; Y must be even."
  (unless (oddp x)
    (incf x)
    (format t
            "~&Changing X to ~S to make it odd." x))
  (when (oddp y)
    (decf y)
    (format t
            "~&Changing Y to ~S to make it even." y))
  (* x y))
(picky-multiply 4 6)
(picky-multiply 2 9)

(defun make-board ()
  (list 'board 0 0 0 0 0 0 0 0 0))
(defun convert-to-letter (v)
  (cond ((equal v 1) "O")
        ((equal v 10) "X")
        (t " ")))
(defun print-row (x y z)
  (format t "~&  ~A | ~A | ~A"
          (convert-to-letter x)
          (convert-to-letter y)
          (convert-to-letter z)))
(defun print-board (board)
  (format t "~%")
  (print-row
   (nth 1 board) (nth 2 board) (nth 3 board))
  (format t "~& -----------")
  (print-row
   (nth 4 board) (nth 5 board) (nth 6 board))
  (format t "~& -----------")
  (print-row
   (nth 7 board) (nth 8 board) (nth 9 board))
  (format t "~%~%"))
(defvar b (make-board))
(print-board b)
(defun make-move (player pos board)
  (setf (nth pos board) player)
  board)
(defvar *computer* 10)
(defvar *opponent* 1)
(make-move *opponent* 3 b)
(make-move *computer* 5 b)
(print-board b)

"P.341 11. Iteration and Block Structure"
(dotimes (i 4)
  (format t "~&I is ~S." i))
(dolist (x '(red blue green) 'flowers)
  (format t "~&Roses are ~S." x))

(defun find-first-odd (list-of-numbers)
  (dolist (e list-of-numbers)
    (format t "~&Testing ~S..." e)
    (when (oddp e)
      (format t "found an odd number.")
      (return e))))
(find-first-odd '(2 4 6 7 8))
(find-first-odd '(2 4 6 8 10))

(defun check-all-odd (list-of-numbers)
  (dolist (e list-of-numbers t)
    (format t "~&Checking ~S..." e)
    (if (not (oddp e)) (return nil))))
(check-all-odd '(1 3 5))

(defun rec-ffo (x)
  "Recursively find first odd number in a list."
  (cond ((null x) nil)
        ((oddp (first x)) (first x))
        (t (rec-ffo (rest x)))))
(defun it-ffo (list-of-numbers)
  "Iteratively find first odd number in a list."
  (dolist (e list-of-numbers)
    (if (oddp e) (return e))))

(defun it-fact (n)
  (let ((prod 1))
    (dotimes (i n prod)
      (setf prod (* prod (+ i 1))))))
(it-fact 10)

(defun it-intersection (x y)
  (let ((result-set nil))
    (dolist (element x result-set)
      (when (member element y)
        (push element result-set)))))
(it-intersection '(f a c e)
                 '(c l o v e))

(defun launch (n)
  (do ((cnt n (- cnt 1)))
      ((zerop cnt) (format t "Blast off!"))
    (format t "~S..." cnt)))
(launch 10)

(defun find-matching-elements (x y)
  "Search X and Y for elements that match."
  (do ((x1 x (rest x1))
       (y1 y (rest y1)))
      ((or (null x1) (null y1)) nil)
    (if (equal (first x1)
               (first y1))
        (return (first x1)))))
(find-matching-elements
 '(b i r d)
 '(c a r p e t))

(defun ffo-with-do (list-of-numbers)
  (do ((x list-of-numbers (rest x)))
      ((null x) nil)
    (if (oddp (first x)) (return (first x)))))
(defun ffo-with-do* (list-of-numbers)
  (do* ((x list-of-numbers (rest x))
        (e (first x) (first x)))
       ((null x) nil)
    (if (oddp e) (return e))))

(defun find-first-odd (x)
  (format t "~&Searching for an odd number...")
  (dolist (element x)
    (when (oddp element)
      (format t "~&Found ~S." element)
      (return-from find-first-odd element)))
  (format t "~&None found.")
  'none)
(find-first-odd '(2 4 6 7 8))
(find-first-odd '(2 4 6 8))

(defun square-list (x)
  (mapcar
   #'(lambda (e)
        (if (numberp e)
            (* e e)
            (return-from square-list 'nope)))
      x))
(square-list '(1 2 3 4 5))
(square-list '(1 2 three four 5))

"P.358 LISP TOOLKIT time"
(defun addup (n)
  "Adds up the first N integers"
  (do ((i 0 (+ i 1))
       (sum 0 (+ sum i)))
      ((> i n) sum)))
(time (addup 1000))
(time (addup 10000))

"These forms are used infrequently today."
(prog1 (defvar x 'foo)
  (setf x 'bar)
  (setf x 'baz)
  (format t "~&X is ~S" x))
(prog2 (defvar x 'foo)
    (setf x 'bar)
  (setf x 'baz)
  (format t "~&X is ~S" x))
(progn (defvar x 'foo)
       (setf x 'bar)
       (setf x 'baz)
       (format t "~&X is ~S" x))

"P.365 12 Structures and The Type System"
(typep 3 'number)
(typep 3 'integer)
(typep 3 'float)
(typep 'foo 'symbol)

(type-of 'aardvark)
(type-of 3.5)
(type-of '(bat breath))
(type-of "Phooey")

(defstruct starship
  (name nil)
  (speed 0)
  (condition 'green)
  (shields 'down))
(defvar s1 (make-starship))
s1
(defvar s2 '#s(starship speed (warp 3)
                        condition red
                        shields up))
s2
(starship-p s2)
(starship-p 'foo)
(typep s1 'starship)
(type-of s2)
(starship-speed s2)
(starship-shields s2)
(setf (starship-name s1) "Enterprise")
(incf (starship-speed s1))
s1

(defun alert (x)
  (setf (starship-shields x) 'up)
  (if (equal (starship-condition x) 'green)
      (setf (starship-condition x) 'yellow))
  'shields-raised)
(alert s1)
s1

(defvar s3 (make-starship :name "Reliant"
                          :shields 'damaged))

;(defstruct starship
;  (captain nil)
;  (name nil)
;  (shields 'down)
;  (condition 'green)
;  (speed 0))
;(defvar s3 (make-starship :captain "Benson"
;                          :name "Reliant"
;                          :shields 'damaged))

"P.372 Lisp Toolkit: DESCRIBE and INSPECT"
(describe #'cons)
(describe 7)
(describe 'fred)
(describe t)
(describe s1)
(inspect s1)

(defvar s5 (make-starship))
(defvar s6 (make-starship))
(equal s5 s6)
(equal s6 s6)

(equalp s5 s6)
(equalp s5 '#s(starship :name nil
                        :speed 0
                        :condition green
                        :shields down))
(equal "enterprise" "Enterprise")
(equalp "enterprise" "Enterprise")

(defstruct ship
  (name nil)
  (captain nil)
  (crew-size nil))
(defstruct (starship (:include ship))
  (weapons nil)
  (shields nil))
(defstruct (supply-ship (:include ship))
  (cargo nil))
(defvar z1 (make-starship
            :captain "James T. Kirk"))
(defvar z2 (make-supply-ship
            :captain "Harry Mudd"))
(ship-p z1)
(starship-p z1)
(supply-ship-p z1)
(ship-captain z1)
(starship-captain z1)
;(supply-ship-captain z1)

"P.383 13 Arrays, Hash Tables, And Property Lists"
(defvar my-vec '#(tuning violin 440 a))
(vectorp my-vec)
(aref my-vec 1)
(defvar a '#(nil nil nil nil nil))
(setf (aref a 0) 'foo)
(setf (aref a 1) 37)
(setf (aref a 2) 'bar)
a
(length a)
(reverse a)
(find-if #'numberp a)

(make-array 5 :initial-element 1)

(length "Cockatoo")
(reverse "Cockatoo")
(aref "Cockatoo" 3)
#\k
(type-of #\k)
(defvar pet "Cockatoo")
(setf (aref pet 5) #\p)
pet

(defvar h (make-hash-table))
(type-of h)
(setf (gethash 'john h)
      '(attorney (16 maple drive)))
(setf (gethash 'mary h)
      '(physician (23 cedar court)))
(gethash 'john h)
(gethash 'bill h)
h
(describe h)

"P.400 Lisp Toolkit: ROOM"
(gc)
(room)
(coerce "Cockatoo" 'list)
(coerce '(#\b #\i #\r #\d) 'string)
(coerce '(foo bar baz) 'vector)
(make-array 3 :element-type 'string-char
              :initial-contents '(#\M #\o #\m))
(map 'list #'+
     '(1 2 3 4)
     '#(10 20 30 40))
(map 'list #'list
     '(a b c)
     '#(1 2 3)
     "xyz")

"P.404 14 Macros and Compilation"
(defvar a 1)
(incf a)
(macroexpand-1 '(incf a))

(defmacro simple-incf (var)
  (list 'setq var (list '+ var 1)))
(defvar a 1)
(simple-incf a)
(macroexpand-1 '(simple-incf a))

(defmacro simple-incf (var &optional (amount 1))
  (list 'setq var (list '+ var amount)))
(defvar b 2)
(macroexpand-1 '(simple-incf b (* 3 a)))

(defun faulty-incf (var)
  (setq var (+ var 1)))

(defvar a 7)
(faulty-incf a)
(faulty-incf a)
a

"There are three important
differences between ordinary functions and macro functions:
1. The arguments to ordinary functions are always evaluated; the arguments to
macro functions are not evaluated.
2. The result of an ordinary function can be anything at all; the result returned
by a macro function must be a valid Lisp expression.
3. After a macro function returns an expression, that expression is
immediately evaluated. The results returned by ordinary functions do not
get evaluated."

"backquote"
(defvar name 'fred)
`(this is ,name from pittsburgh)
`(i gave ,name about ,(* 25 8) dollars)

(defmacro simple-incf (var &optional (amount 1))
  `(setq ,var (+ ,var ,amount)))
(macroexpand-1 '(simple-incf fred-loan (* 25 8)))

(defmacro two-from-one (func object)
  `(,func ',object ',object))
(two-from-one cons aardvark)
(macroexpand-1 '(two-from-one cons aardvark))

(defmacro showvar (var)
  `(format t "~&The value of ~S is ~S"
           ',var
           ,var))
(defun f (x y)
  (showvar x)
  (showvar y)
  (* x y))
(f 3 7)

(defvar name 'fred)
(defvar address '(16 maple drive))
`(,name lives at ,address now)
`(,name lives at ,@address now)

(defmacro set-zero (&rest variables)
  `(progn ,@(mapcar #'(lambda (var)
                        (list 'defvar var 0))
                    variables)
          '(zeroed ,@variables)))
(set-zero a b c)
(macroexpand-1 '(set-zero a b c))

(defun tedious-sqrt (n)
  (dotimes (i n)
    (if (> (* i i) n) (return i))))
(time (tedious-sqrt 5000000))
(compile 'tedious-sqrt)
(time (tedious-sqrt 5000000))

(defmacro bad-announce-macro ()
  (format t "~%Hi mom!"))
(defun bad-say-hi ()
  (bad-announce-macro))
(compile 'bad-say-hi)
(bad-say-hi)
(defmacro good-announce-macro ()
  `(format t "~%Hi mom!"))
(defun good-say-hi ()
  (good-announce-macro))
(compile 'good-say-hi)
(good-say-hi)

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
          (exp (macroexpand exp1))
          (*print-circle* nil))
     (cond ((equal exp exp1)
            (format t "~&Macro expansion:")
            (pprint exp))
           (t (format t "~&First step of expansion:")
              (pprint exp1)
              (format t "~%~%Final expansion:")
              (pprint exp)))
     (format t "~%~%")
     (values)))

(defmacro lengthy-incf (var)
  `(setf ,var (+ ,var 1)))
(ppmx (lengthy-incf a))
(ppmx (dotimes (i n)
        (if (> (* i i) n) (return i))))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun next-power-of-two (n &aux (i 1))
  (while (< i n)
    (format t "~&Not ~S" i)
    (setf i (* i 2)))
  i)
(next-power-of-two 11)

(defmacro mix-and-match (p q)
  (let ((x1 (first p))
        (y1 (second p))
        (x2 (first q))
        (y2 (second q)))
    `(list '(,x1 ,y1)
           '(,x1 ,y2)
           '(,x2 ,y1)
           '(,x2 ,y2))))
(mix-and-match (fred wilma) (barney betty))

(defmacro dovector ((var vector-exp
                     &optional result-form)
                    &body body)
  `(do* ((vec-dov ,vector-exp)
         (len-dov (length vec-dov))
         (i-dov 0 (+ i-dov 1))
         (,var nil))
        ((equal i-dov len-dov) ,result-form)
     (setf ,var (aref vec-dov i-dov))
     ,@body))
(dovector (x '#(foo bar baz))
  (format t "~&X is ~S" x))
(ppmx (dovector (x '#(foo bar baz))
        (format t "~&X is ~S" x)))
