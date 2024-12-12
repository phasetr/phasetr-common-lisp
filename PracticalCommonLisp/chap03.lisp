;; http://www.gigamonkeys.com/book/practical-a-simple-database.html
;; "CLOS = Common Lisp Object System"
(list 1 2 3)
;; plist
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

(format t "~a" "Dixie Chicks")
(format t "~a" :title)
(format t "~a:~10t~a" :artist "Dixie Chicks")

(defun dump-db2 ()
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

;; Improving the User Interaction
(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd1 ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "Ripped [y/n]")))

;(parse-integer (prompt-read "Rating"))
;(parse-integer (prompt-read "Rating") :junk-allowed t)
;(or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
;(y-or-n-p "Ripped [y/n]: ")

(defun prompt-for-cd2 ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd2))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

;; Saving and Loading the Database
(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; #' は変数名ではなく関数名だと伝えるための記号
(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
(remove-if-not #'(lambda (x) (= 0 (mod x 2)))
               '(1 2 3 4 5 6 7 8 9 10)) ; => (2 4 6 8 10)
(remove-if-not #'(lambda (x) (= 1 (mod x 2))) '
               (1 2 3 4 5 6 7 8 9 10)) ; => (1 3 5 7 9)

(remove-if-not
 #'(lambda (cd)
     (equal (getf cd :artist) "Dixie Chicks"))
 *db*)

(defun select-by-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(select
 #'(lambda (cd)
     (equal (getf cd :artist) "Dixie Chicks")))

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(select (artist-selector "Dixie Chicks"))

(defun foo1 (a b c) (list a b c))
(defun foo2 (&key a b c) (list a b c))
(foo2 :a 1 :b 2 :c 3) ; => (1 2 3)
(foo2 :c 3 :b 2 :a 1) ; => (1 2 3)
(foo2 :a 1 :c 3)      ; => (1 NIL 3)
(foo2)                ; => (NIL NIL NIL)

(defun foo3
    (&key a (b 20) (c 30 c-p))
  (list a b c c-p))
(foo3 :a 1 :b 2 :c 3) ; => (1 2 3 T)
(foo3 :c 3 :b 2 :a 1) ; => (1 2 3 T)
(foo3 :a 1 :c 3)      ; => (1 20 3 T)
(foo3)                ; => (NIL 20 30 NIL)

(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; Updating Existing Records--Another Use for WHERE
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

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

(defmacro backwards (expr) (reverse expr))
(backwards ("hello, world" t format))
