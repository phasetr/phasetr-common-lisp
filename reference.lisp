(ql:quickload :rove)
(use-package :rove)

#|
複数行コメント
コンパイルテスト: C-c C-c すると SLIME で読み込めるようになる. C-c C-z で SLIME に移れる

コメントのルール
セミコロン 1 つ: 行末コメント
セミコロン 2 つ: 関数内の独立行コメント
セミコロン 3 つ: 関数外のコメント"
|#

(progn
  (defun fizzbuzz (n)
    (if (or (zerop (mod n 3))
          (zerop (mod n 5)))
      (progn
        (when (and (zerop (mod n 3)))
          (format t "fizz"))
        (when (and (zerop (mod n 5)))
          (format t "buzz"))
        (format t " "))
      (format t "~a " n)))
  (dotimes (n 20) (fizzbuzz (1+ n))))

(progn
  (defun m/s-to-km/h (m/s) (float (* m/s 60 60 (/ 1 1000))))
  (defun km/h-to-m/s (km/h) (float (* km/h 1000 (/ 1 3600))))
  (let ((x 1.0))
    (testing "m/s <-> km/h"
    (ok (m/s-to-km/h x) 3.6)
    (ok (km/h-to-m/s 3.6) x)
    (ok (m/s-to-km/h (km/h-to-m/s x)) x)
    (ok (km/h-to-m/s (m/s-to-km/h x)) x))))

(progn
  "misc"
  (describe #(1 2))
  (describe #2A((1 2) (3 4)))
  (print (documentation 'cons 'function))
  (print (documentation '*print-length* 'variable))
  (print "test")
  (prin1 "test")
  (princ "test")
  (print (format nil "~A" #(1 2 3)))
  (format t "~A~%" #(1 2 3)))

(progn
  "flet"
  (flet ((foo (a b c) a))
    (let ((res (foo :a :b :c)))
      (print res)))
  (flet ((foo (a b c)
           (values a b c)))
    (let ((res (foo :a :b :c)))
      (print res)))
  (flet ((foo (a b c)
           (values a b c)))
    (multiple-value-bind (res1 res2 res3) (foo :a :b :c)
      (format t "res1 is ~a, res2 is ~a, res2 is ~a~&" res1 res2 res3))))

(progn
  (let*
      ((foo 2) (bar foo))
    (print foo)
    (print bar)))

(testing "array-test"
  (testing "length"
    (ok (eq 2 (length #(1 2)))))
  (testing "make-array"
    (ok (equalp (make-array 3 :initial-contents '(1 2 3)) #(1 2 3)))
    (ok (equalp (vector 1 2 3) #(1 2 3)))
    (ok (not (equalp (vector 1 2 3) #(1 2)))))
  (testing "2dim-array"
    (ok (equalp (make-array '(2 3) :initial-contents '((1 2 3) (4 5 6)))
                #2A((1 2 3) (4 5 6))))
    (testing "2dim-array aref"
      (let ((xa #2A((0 1) (2 3))))
        (ok (eq 0 (aref xa 0 0)))
        (ok (eq 1 (aref xa 0 1)))
        (ok (eq 2 (aref xa 1 0)))
        (ok (eq 3 (aref xa 1 1)))))
    (testing "2dim-array transpose"
      (defun nth-column-2darray (n xa)
        (let ((j (first (array-dimensions xa))))
          (apply #'vector
                 (loop for i from 0 to (1- j)
                       collect (aref xa i n)))))
      (let ((xa #2A((0 1 2) (3 4 5))))
        (ok (equalp #(0 3)
                    (nth-column-2darray 0 xa))))))
  (testing "aref: 引数の順番に注意"
    (ok (eq (aref #(1 2) 0) 1))
    (ok (eq (aref #(1 2) 1) 2))))

(testing "cl-ppcre"
  (ok (equal
       (cl-ppcre:regex-replace "fo+" "foo bar" "frob")
       "frob bar"))
  (ok (equal
       (multiple-value-bind (a b) (cl-ppcre:regex-replace "fo+" "foo bar" "frob")
         (list a b))
       '("frob bar" T))))

(testing "cond-test"
  (testing "cond"
      (ok (= (let ((a 1))
               (cond ((= a 1) 1)
                     ((= a 2) 2)
                     (t 'TRUE)))
             1))
      (ok (= (let ((a 2))
               (cond ((= a 1) 1) ((= a 2) 2) (t 'TRUE)))
             2))
      (ok (= (let ((a 2))
               (cond ((= a 1) 1) ((= a 2) 2) (t 'TRUE)))
             'TRUE)))
  (testing "if"
    (ok (eq (if (oddp 1) 'odd 'even) 'odd))
    (ok (eq (if (oddp 2) 'odd 'even) 'even))))

(testing "eq-test: use `=` for integer, and `equal` for others."
  (ok (eq 1 1) t)
  (ok (not (eq 1 2)) t)
  (ok (eq 'a 'a) t)
  (ok (not (eq 'a 'b)) t)
  (ok (equal "test" "test") t)
  (ok (not (equal "test" "t")) t)
  (testing "string equals"
    (testing "string=: 大文字, 小文字, スペースの違いも区別"
      (ok (string= "Michael" "Michael"))
      (ok (eq (string= "michael" "Michael") nil))
      (ok (eq (string= "ToyPoodle" "Toy Poodle") nil)))
    (testing "string/=: ２つの引数が違う文字列のとき真, 同じ文字の数が返る"
      (ok (eq (string/= "google" "goo") 3))
      (ok (eq (string/= "dog" "cat") 0))
      (ok (eq (string/= "Common Lisp" "Common Lisp") nil)))
    (testing "string-equal: 大文字と小文字の違いを区別しないがスペースは区別する"
      (ok (string-equal "Mozart" "MOZART"))
      (ok (eq (string-equal "HopStepJump" "Hop Step Jump") nil)))
    (testing "string<, string>: 文字数の多少を比較. 真のとき同じ文字数を返す: 数字が返るときは真. 偽のときNIL. 大文字・小文字の区別あり.
一文字目が異なる文字で以降に同じ文字が続いていても偽."
      (ok (eq (string> "psychology" "psycho") 6))
      (ok (eq (string> "Psychology" "psycho") nil)))
    (testing "string<=, string>=: ２つの引数が同じ文字列のときも真, 同じ文字数が数値として返る"
      (ok (eq (string>= "triangle" "Triangle") 0))
      (ok (eq (string>= "Triangle" "triangle") nil)))))

(testing "format-test"
  (testing "string"
    (ok (equal (format nil "~a" "ABCD") "ABCD"))))

(testing "function-test"
  (testing "difference #' and '"
    (defun foo (x) (* x 100))
    (flet ((foo (x) (1+ x)))
      (ok '(funcall #'foo 1) 2)
      ;; `#'`はローカル関数を呼ぶ
      (ok (macroexpand #'foo) '(function foo)))
    (flet ((foo (x) (1+ x)))
      (ok '(funcall 'foo 1) 101)
      ;; `'`はグローバル関数を呼ぶ
      (ok (macroexpand 'foo) 'foo)))
  (testing "apply, funcall"
    (ok (funcall #'+ 1 2) 3)
    (ok (apply #'+ '(1 2)) 3))
  (testing "flet"
    (let ((x 1))
      (ok x 1)
      (defun f () '(1 2 3))
      (flet ((f () '(1 2 3 4)))
        (ok (f) '(1 2 3 4)))
      (ok (f) '(1 2 3))))
  (testing "lambda-test"
    (ok '((lambda (x) x) "hello") "hello"))
  "trace: 再帰関数のチェックに役立つ")

(testing "io-test"
  (ok (outputs (write-string "a") "a"))
  (outputs (print "a") "a")
  (testing "print-test"
    (prin1 '(1 2 3))
    (princ '(1 2 3))
    (print '(1 2 3))
    (pprint '(1 2 3))))

(testing "list-test"
  (testing "append"
    (ok (equal (append '(friends romans) '(and countrymen))
               '(friends romans and countrymen)))
    (ok (equal (append '(w x y) 'z) '(w x y . z)))
    (ok (equal (append '(a b c) '(d)) '(a b c d))))
  (testing "apply"
    (ok (equal (apply #'+ '(1 2 3)) 6)))
  (testing "car-cdr"
    (ok (equal (car '(1 2 3)) 1))
    (ok (equal (cdr '(1 2 3)) '(2 3)))
    (ok (equal (cdr '(1)) nil))
    (ok (eq (car nil) nil))
    (ok (eq (cdr nil) nil))
    (let* ((a '((1 2 3) . 4)))
      (ok (equal (car a) '(1 2 3)))
      (ok (equal (cdr a) 4))))
  (testing "cons"
    (ok (eq 1 (car '(1 . 2))))
    (ok (eq 2 (cdr '(1 . 2))))
    (ok (equal (list 'foo 'bar 'baz) '(foo bar baz)))
    (ok (equal (cons 'foo '(bar baz)) '(foo bar baz))))
  (testing "filter"
    (ok (equal (remove-if-not #'evenp '(0 1 2 3)) '(0 2)))
    (ok (equal (remove-if-not #'oddp  '(0 1 2 3)) '(1 3))))
  (testing "find, member"
    (ok (equal (find 1 '(0 1 2)) 1))
    (ok (equal (member 1 '(0 1 2)) '(1 2))))
  (testing "head"
    (ok (eq (car '(1 2 3)) 1)))
  (testing "length"
    (ok (= (length '(1 2 3)) 3))
    (ok (= (length '(a (b c) d)) 3))
    (ok (= (length 'nil) 0))
    (ok (= (length nil) 0))
    (ok (equal (null nil) t))
    (ok (equal (null ()) t)))
  (testing "list to string"
    (ok (equal (format nil "~{~A~}" '(1 2 3)) "123"))
    (ok (equal (format nil "~{~A~^, ~}" '(1 2 3)) "1, 2, 3"))
    (ok (equal (format nil "~{~A~^ ~}" '(1 2 3)) "1 2 3")))
  (testing "map"
    (ok (mapcar #'1+ '(1 2 3)) '(2 3 4))
    (ok (mapcar (lambda (x) (* x x)) '(1 2 3)) '(1 4 9)))
  (testing "multiple-value-list"
    (ok (multiple-value-list (values 1 2 3)) '(1 2 3)))
  (testing "nth-value"
    (let ((a '(1 2 3 4 5)))
      (ok (eq (first a) 1))
      (ok (eq (second a) 2))
      (ok (eq (third a) 3))
      (ok (equal (rest a) '(2 3 4 5))))
    (ok (nth-value 0 (values :a :b :c)) :A)
    (ok (nth-value 2 (values :a :b :c)) :B)
    (ng (nth-value 9 (values :a :b :c)))
    (ok (nth-value 0 '(:a :b :c)) '(:a :b :c)) ;; => (:A :B :C)
    (ng (nth-value 1 '(:a :b :c))))
  (testing "ref-list"
    (ok (eq 1 (first '(1 2 3))))
    (ok (eq 2 (second '(1 2 3))))
    (ok (eq 3 (third '(1 2 3)))))
  (testing "reduce, fold"
    (ok (eq (reduce #'min '(1 2 3)) 1))
    (ok (eq (reduce #'+ '(1 2 3)) 6))
    (ok (eq (reduce #'+ '(1 2 3) :initial-value 1) 7))
    (ok (eq (reduce #'- '(1 2 3) :from-end t) 2))
    (ok (eq (reduce (lambda (old new) (* old new)) '(1 2 3)) 6)))
  (testing "reverse"
    (ok (equal (reverse '(0 1 2)) '(2 1 0))))
  (testing "tail, rest"
    (ok (equal (cdr '(1 2 3)) '(2 3))))
  (testing "zip"
    (defun zip (&rest lists) (apply #'mapcar #'list lists))
    (ok (equal (zip '(1 2) '(3 4)) '((1 3) (2 4))))))

(testing "loop-test"
  (loop :for i :from 3 :upto 5 :do (print i))
  (loop :repeat 5 :do (format t "~&five"))
  (loop :for i :upto 5 :do (format t "~&~A" i))
  (loop :for i :below 5 :collect i)
  (testing "dotimes"
    ;; 返り値を設定しないとnilが返る
    (ok (eq (dotimes (x 5) (print x))
            nil))
    ;; resultを置いておくとresultに設定した値を返す
    (ok (eq (dotimes (x 5 result) (setf result x))
            4)))
  (testing "iota"
    (defun iota (n &optional (start 0))
      "range in lisp"
      (if (zerop n)
          nil
          (cons start (iota (- n 1) (1+ start)))))
    (ok (iota 10)
        (loop for i from 0 to 9 collect i))
    (ok (iota 4 3)
        (loop for i from 3 to 6 collect i)))
  ;; https://codehero.jp/python/13937520/pythons-range-analog-in-common-lisp
  (testing "range"
    (defun range (max &key (min 0) (step 1))
      (loop for n from min below max by step
            collect n))
    (testing "range test"
      (ok (loop for i from 1 to 5 collect i)
          '(1 2 3 4 5))
      (ok (range 5)
          '(0 1 2 3 4))
      (ok (range 10 :min 1 :step 2)
          '(1 3 5 7 9))))
  (ok (equal (loop for i below 3 collect i) '(0 1 2)))
  (ok (equal (loop for i from 1 to 3 collect i) '(1 2 3)))
  (testing "array-loop"
    (ok (equal '(1 2 3)
               (loop for e across #(1 2 3) collect e)))))

(testing "macro"
  (defmacro my-when (cond &body body)
    `(if ,cond
         (progn ,@body)
         nil))
  (macroexpand-1 '(my-when (> num 40)
                 (format t "40!~%")
                 'over-forty)))

(testing "math-test"
  (testing "abs, 絶対値"
    (ok (eq 1 (abs 1)))
    (ok (eq 2 (abs -2))))
  (testing "accessor, 引数の順番に注意"
    (ok (eq (nth 0 '(1 2 3)) 1))
    (ok (eq (nth 1 '(1 2 3)) 2)))
  (testing "arithmetic, 算術"
    (ok (eq (+ 1 2) 3))
    (ok (eq (- 1 2) -1))
    (ok (eq (- 1 2 3) -4))
    (ok (eq (* 2 3) 6))
    (print (/ 3 4))
    (ok (equal (/ 3 4) 3/4)))
  (testing "ceiling, 切上げ"
    (ok (eq (ceiling 1.4) 2))
    (ok (eq (ceiling 1.5) 2)))
  (testing "dec"
    (ok (eq (1- 5) 4))
    (ok (eq (1- 4) 3)))
  (testing "exponential"
    (ok (eq (expt 2 3) 8))
    (ok (eq (expt 3 2) 9)))
  (testing "inc"
    (ok (eq (1+ 5) 6))
    (ok (eq (1+ 6) 7)))
  (testing "incf"
    (let ((x 1))
      (incf x 4)
      (ok x 5)))
  (random 100)
  (random 1.0)
  (testing "round, 四捨五入"
    (ok (eq (round 1.4) 1))
    (ok (eq (round 1.5) 2)))
  (testing "sqrt, 平方根"
    (ok (eq (sqrt 2) 1.4142135))))

(testing "predicative-test"
  (ok (eq (and 'george nil 'harry) nil))
  (ok (eq (and 'george 'fred 'harry) 'harry))
  (ok (eq (and 1 2 3 4 5) 5))
  (ok (eq (or nil t t) t))
  (ok (eq (or 'george nil 'harry) 'george))
  (ok (eq (or 'george 'fred 'harry) 'george))
  (ok (eq (or nil 'fred 'harry) 'fred))
  (ok (eq (atom nil) t))
  (ok (eq (atom '(1 2 3)) nil))
  (ok (eq (consp nil) nil))
  (testing "even-odd"
    (ok (eq (evenp 28) t))
    (ok (eq (oddp 35) t))
    (ok (eq (oddp 2) nil)))
  (ok (eq (listp 'STITCH) nil))
  (ok (eq (listp '('a 'stitch 'in 'time)) t))
  (ok (eq (null nil) t))
  (ok (eq (null '(1 2 3)) nil))
  (ok (eq (numberp 2) t))
  (ok (eq (numberp 'dog) nil))
  (ok (eq (symbolp 'cat) t))
  (ok (eq (symbolp 42) nil))
  (ok (eq (zerop 35) nil))
  (ok (eq (zerop 0) t))
  (ok (eq (< 2 3) t))
  (ok (eq (> 2 3) nil))
  (ok (eq (not t) nil))
  (ok (eq (not nil) t)))

"TODO https://ja.wikibooks.org/wiki/Lisp/基本からさらに一歩進んで/文字列"
"注意: 文字列は文字の配列で配列操作関数が使える"
(testing "string-test"
  (testing "accessing individual characters"
    (ok (equal (char "Groucho Marx" 11) #\x))
    (ok (equal (char "Groucho Marx" 7) #\Space))
    (ok (equal (aref "Groucho Marx" 7) #\Space))
    (ok (equal (elt "Groucho Marx" 7) #\Space)))
  (testing "concatenate"
    (ok (equal (concatenate 'string "Karl" " " "Marx") "Karl Marx"))
    (ok (equal (concatenate 'list "Karl" " " "Marx") (list #\K #\a #\r #\l #\Space #\M #\a #\r #\x)))
    (ok (equal (concatenate 'string '(#\K #\a #\r #\l #\Space #\M #\a #\r #\x)) "Karl Marx")))
  (testing "get element (char by index"
    (ok (eq (elt "ABCD" 0) #\A))
    (ok (eq (elt "abcd" 0) #\a))
    (ok (eq (elt "1234" 0) #\1)))
  (testing "length"
    (ok (eq (parse-integer "1234") 1234)))
  (testing "list-to-string"
    (ok (equal (format nil "~{~A~}" '(1 2 3)) "123")))
  (testing "Processing a String One Character at a Time"
    (map 'string #'(lambda (c) (print c)) "Groucho Marx")
    (ok (equal (loop for char across "Zeppo" collect char)
               '(#\Z #\e #\p #\p #\o))))
  (testing "read-from-string: string to number"
    (ok (= (read-from-string " 1 3 5" t nil :start 2) 3)) ; 3, 5
    (ok (= (read-from-string "1/10") 1/10))
    (ok (= (read-from-string "1.2") 1.2))
    (ok (= (read-from-string "#c(1 1)") #c(1 1))))
  (testing "remove a character"
    (ok (equal (remove #\o "Harpo Marx") "Harp Marx"))
    (ok (equal (remove #\o "Harpo Marx" :start 2) "Harp Marx"))
    (ok (equal (remove-if #'upper-case-p "Harpo Marx") "arpo arx"))
    (ok (equal (substitute #\u #\o "Groucho Marx") "Gruuchu Marx"))
    (ok (equal (substitute-if #\_ #'upper-case-p "Groucho Marx") "_roucho _arx"))
    (ok (equal (replace "Zeppo Marx" "Harpo" :end1 5) "Harpo Marx")))
  (testing "search"
    (ok (eq 7 (search "dog" "it's a dog's life")))
    (ok (eq 2 (search '(0 1) '(2 4 6 1 3 5) :key #'oddp))))
  (testing "string to char: 文字列から文字を取り出す"
    (let ((s "1234")
          (ss (vector "1234" "5678")))
      (ok (equal #\1 (char s 0)))
      (ok (equal #\1 (aref s 0)))
      ;;(ok (equal #\1 (aref ss 0 0))) ;; ERROR!
      (ok (equal #\1 (aref (aref ss 0) 0)))
      (ok (equal #\2 (aref (aref ss 0) 1)))))
  (testing "string to integer"
    (ok (= (parse-integer "1234") 1234))
    (ok (= (parse-integer "11" :radix 2) 3))
    (ok (= (parse-integer "11" :radix 8) 9))
    (ok (= (parse-integer "11" :radix 10) 11))
    (ok (= (parse-integer "11" :radix 16) 17))
    (let ((data '("174円" "103円" "190円" "121円" "139円" "157円" "192円" "197円" "110円" "129円"
                  "105円" "131円" "139円" "123円" "179円" "195円" "110円" "122円" "140円" "151円")))
      (ok (equal (mapcar (lambda (x) (parse-integer x :junk-allowed T)) data)
                 '(174 103 190 121 139 157 192 197 110 129 105 131 139 123 179 195 110 122 140 151))))
    (let ((data '("単価189円" "単価134円" "単価114円" "単価125円" "単価199円" "単価123円" "単価107円"
                  "単価162円" "単価177円" "単価122円" "単価172円" "単価121円" "単価131円" "単価197円"
                  "単価108円" "単価152円" "単価102円" "単価197円" "単価168円" "単価102円")))
      (ok (equal (mapcar (lambda (x) (parse-integer x :junk-allowed T
                                                      :start (position-if #'digit-char-p x)))
                         data)
                 '(189 134 114 125 199 123 107 162 177 122 172 121 131 197 108 152 102 197 168 102)))))
  (testing "substring"
    (ok (equal (subseq "now string" 8) "ng"))
    (ok (equal (subseq "now string" 0 7) "now str"))))

(testing "vector-test"
  (ok (eq 1 (aref (vector 1 2) 0))))

(testing "library Arrow-macros-test"
  (ok (eq 10
          (-> 1
            (+ 2 3)
            #'1+
            1+
            (lambda (x) (+ x 1)) (1+))))
  (ok (eq 20
          (->> (list 1 2 3 4 5)
            (mapcar #'1+)
            (reduce #'+))))
  (ok (equal "Qponmlkj"
             (-<> "abcdefghijklmnopqrstuvwxyz"
               (ppcre:scan-to-strings "j.*q" <>)
               (sort #'string>)
               (string-upcase :end 1)))))
