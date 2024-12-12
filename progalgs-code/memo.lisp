(ql:quickload :rutils)
(named-readtables:in-readtable rtl:rutils-readtable)

;;; P.43
(defstruct pair
  left right)
(make-pair :left "foo" :right "bar")
(pair-right (read-from-string
             (prin1-to-string
              (make-pair :left "foo" :right "bar"))))

;(defstruct (pair (:type list) (:conc-name nil))
;  "A generic pair with left (LT) and right (RT) elements."
;  lt rt)
(defun pair (x y)
  "A shortcut to make a pair of X and Y."
  (make-pair :lt x :rt y))

(let ((x 1)
      (a (values 1 2)))
  (print x)
  (format nil "~A" a))

(make-array 3)
(make-array 3 :element-type 'list :initial-element nil)
(make-array 3 :initial-contents '(1.0 2.0 3.0))
(let ((a #(1.0 2.0 3.0)))
  (format nil "~A" a))

(let ((vec (make-array 3 :initial-contents '(1.0 2.0 3.0))))
  (print (aref vec 0)))
(let ((vec (make-array 3 :initial-contents '(1.0 2.0 3.0))))
  (print (aref vec 0))
  (print (aref vec 1))
  (setf (aref vec 2) 4.0)
  (print (aref vec 2)))

"P.42"
;;; warning
;(let ((vec #(1.0 2.0 3.0)))
;  (setf (aref vec 2) nil)
;  (format nil "~A" vec))

;;; warning
;(rtl:with ((vec (rtl:vec 1 2 3))
;           (part (rtl:slice vec 2)))
;  (print part)
;  (setf (aref part 0) 4)
;  (print part)
;  (print vec))

;;; P.55
(defun map-vec (fn vec)
  (let ((rez (make-array (length vec))))
    (dotimes (i (length vec))
      (setf (aref rez i) (funcall fn (aref vec i))))
    rez))
(print (map-vec '1+ #(1 2 3)))
(describe (map-vec '1+ #(1 2 3)))
(print (mapcar #'1+ '(1 2 3)))
(describe (mapcar #'1+ '(1 2 3)))

;;; P.46
(defun clumsy-filter-vec (pred vec)
  "Return the vector with only those elements of VEC
 for which calling pred returns true."
  (let ((rez (make-array (length vec) :fill-pointer 0)))
    (dotimes (i (length vec))
      (when (funcall pred (aref vec i))
        (vector-push (aref vec i) rez)))
    rez))
(describe (clumsy-filter-vec 'oddp #(1 2 3)))

;;; P.56
(let ((vec (make-array 0 :fill-pointer t :adjustable t)))
  (dotimes (i 10)
    (vector-push-extend i vec)
    (describe vec)))

"P.61"
(let ((vec (make-array 2 :initial-contents
                       (list (list :foo :bar)
                             (list :baz :quux)))))
  (print (find (list :foo :bar) vec))
  (print (find (list :foo :bar) vec :test 'equal))
  (print (find (list :bar :baz) vec :test 'equal)))

"P.56"
(defun bin-search (val vec &optional (pos 0))
  (if (> (length vec) 1)
      (rtl:with ((mid (floor (length vec) 2))
                 (cur (aref vec mid)))
        (cond ((< cur val) (bin-search val
                                       (rtl:slice vec mid)
                                       (+ pos mid)))
              ((> cur val) (bin-search val
                                       (rtl:slice vec 0 mid)
                                       pos))
              (t (+ pos mid))))
      (when (= (aref vec 0) val)
        pos)))
(rtl:slice #(1 2 3 4 5) 3)

(print most-positive-fixnum)
(print (type-of most-positive-fixnum))
(print (+ most-positive-fixnum most-positive-fixnum))
(print (type-of (+ most-positive-fixnum most-positive-fixnum)))

"P.65"
(defun bin-search (val vec &key (less '<) (test '=) (key 'identity))
  (when (plusp (length vec))
    (let ((beg 0)
          (end (1- (length vec))))
      (do ()
          ((= beg end))
        (let ((mid (+ beg (floor (- end beg) 2))))
          (if (funcall less (funcall key (aref vec mid)) val)
              (setf beg (1+ mid))
              (setf end mid))))
      (values (aref vec beg)
              beg
              (funcall test (funcall key (aref vec beg)) val)))))

"P.57"
(rtl:with ((size 1000)
           (mid (1+ (/ size 2)))
           (vec (make-array size)))
  (dotimes (i size)
    (setf (aref vec i) i))
  (time (find mid vec))
  (time (bin-search mid vec)))

"P.62"
(defun bogosort (vec comp)
  (dolist (variant (all-permutations vec))
    (dotimes (i (1- (length variant)))
      ;; this is the 3rd optional argument of dotimes header
      ;; that is evaluated only after the loop finishes normally
      ;; if it does we have found a completely sorted
      permutation!
      (return-from bogosort variant))
    (when (funcall comp (aref variant (1+ i)) (aref variant i))
      (return)))) ; current variant is not sorted, skip it


"P.63"
(defun selection-sort (vec comp)
  (dotimes (i (1- (length vec)))
    (let ((best (aref vec i))
          (idx i))
      (dotimes (j (- (length vec) i 1))
        (when (funcall comp (aref vec (+ i j 1)) best)
          (setf best (aref vec (+ i j 1))
                idx (+ i j 1))))
      (rotatef (aref vec i) (aref vec idx)))) ; this is the Lisp swap operator
  vec)
(ok (selection-sort #(3 2 1) #'<) #(1 2 3))

"P.64"
(defun insertion-sort (vec comp)
  (dotimes (i (1- (length vec)))
    (do ((j i (1- j)))
        ((minusp j))
      (if (funcall comp (aref vec (1+ j)) (aref vec j))
          (rotatef (aref vec (1+ j)) (aref vec j))
          (return))))
  vec)
(ok (insertion-sort #(3 2 1) #'<) #(1 2 3))

"P.66"
(defun quicksort (vec comp)
  (when (> (length vec) 1)
    (rtl:with ((pivot-i 0)
           (pivot (aref vec (1- (length vec)))))
      (dotimes (i (1- (length vec)))
        (when (funcall comp (aref vec i) pivot)
          (rotatef (aref vec i)
                   (aref vec pivot-i))
          (incf pivot-i)))
      ;; swap the pivot (last element) in its proper place
      (rotatef (aref vec (1- (length vec)))
               (aref vec pivot-i))
      (quicksort (rtl:slice vec 0 pivot-i) comp)
      (quicksort (rtl:slice vec (1+ pivot-i)) comp)))
  vec)
(ok (quicksort #(3 2 1) #'<) #(1 2 3))

(defun random-vec (size)
  (let ((vec (make-array size)))
    (dotimes (i size)
      (setf (aref vec i) (random size)))
    vec))
(print (random-vec 100))

"P.75"
(print '("hello" world 111))
(print (make-list 3))
(print (list "hello" 'world 111))

"P.76"
(defstruct list-cell data next size)
(defstruct our-own-list
  (head nil :type (or list-cell null))
  (tail nil :type (or list-cell null))
  (size 0 :type (integer 0)))
(let ((tail (make-list-cell :data "world")))
  (make-our-own-list
   :head (make-list-cell
          :data "hello"
          :next tail
          :size 0)
   :tail tail))

(map 'vector '+ '(1 2 3 4 5) #(1 2 3))

"P.79"
(defun dwim-map (fn seq &rest seqs)
  "A thin wrapper over MAP that uses the type of the first SEQ for the result."
  (apply 'map (type-of seq) fn seqs))
"cf. mapcar, mapc, mapcan"
(print
 (mapcar (lambda (x) (when (oddp x) x))
         (rtl:range 1 10)))
(ok
 (remove
  nil
  (mapcar (lambda (x) (when (oddp x) x))
          (rtl:range 1 10)))
 '(1 3 5 7 9))
(ok
 (remove-if-not 'oddp (rtl:range 1 10))
 '(1 3 5 7 9))

"P.81"
(let ((l1 (list 1 2 3))
      (l2 (list 4 5 6)))
  (ok l1 '(1 2 3))
  (append l1 l2) ; note no assignment to l1
  (ok l1 '(1 2 3))
  (ok l2 '(4 5 6)))
"P.82"
(let ((l1 (list 1 2 3)))
  (print (nthcdr 0 l1))
  (print (nthcdr 1 l1))
  (print (nthcdr 2 l1))
  (print (cons 4 (nthcdr 1 l1)))
  (rplacd (nthcdr 0 l1)
          (cons 4 (nthcdr 1 l1)))
  (ok l1 '(1 2 3 4)))

"P.83"
(defstruct (list-cell2 (:include list-cell))
  prev)
(defun our-cons2 (data list)
  (when (null list) (setf list (make-our-own-list)))
  (let ((new-head (make-list-cell2
                   :data data
                   :next (rtl:? list 'head))))
    (when (rtl:? list 'head)
      (setf (rtl:? list 'head 'prev) new-head))
    (make-our-own-list
     :head new-head
     :tail (rtl:? list 'tail)
     :size (1+ (rtl:? list 'size)))))

"P.85"
(defun simple-mapcar-v2 (fn list)
  (let ((rez (list)))
    (dolist (item list)
      (push (funcall fn item) rez))
    (reverse rez)))

"P.86"
(print 1)
(let ((*standard-output* (make-broadcast-stream)))
  (print 1))

(defstruct queue
  head
  tail)
(defun enqueue (item queue)
  (push item (rtl:? queue 'head)))
(defun dequeue (queue)
  (unless (rtl:? queue 'tail)
    (do ()
        ((null (rtl:? queue 'head)))
      (push (pop (rtl:? queue 'head)) (rtl:? queue 'tail))))
  (when (rtl:? queue 'tail)
    (values (pop (rtl:? queue 'tail))
            t)))
(let ((q (make-queue)))
  (ok q (read-from-string "#S(QUEUE :HEAD NIL :TAIL NIL)"))
  (enqueue 1 q)
  (enqueue 2 q)
  (enqueue 3 q)
  (ok q (read-from-string "#S(QUEUE :HEAD (3 2 1) :TAIL NIL)"))
  (dequeue q)
  (ok q (read-from-string "#S(QUEUE :HEAD NIL :TAIL (2 3))"))
  (enqueue 4 q)
  (ok q (read-from-string "#S(QUEUE :HEAD (4) :TAIL (2 3))"))
  (dequeue q)
  (ok q (read-from-string "#S(QUEUE :HEAD (4) :TAIL (3))"))
  (dequeue q)
  (ok q (read-from-string "#S(QUEUE :HEAD (4) :TAIL NIL)"))
  (dequeue q)
  (ok q (read-from-string "#S(QUEUE :HEAD NIL :TAIL NIL)")))

"P.88"
(defun arith-eval (expr)
  "EXPR is a list of symbols that may include:
   square brackets, arithmetic operations, and numbers."
  (let ((ops ())
        (vals ())
        (op nil)
        (val nil))
    (dolist (item expr)
      (case item
        ([ ) ; do nothing
        ((+ - * /) (push item ops))
        (] (setf op (pop ops)
                 val (pop vals))
         (case op
           (+ (incf val (pop vals)))
           (- (decf val (pop vals)))
           (* (setf val (* val (pop vals))))
           (/ (setf val (/ val (pop vals)))))
         (push val vals))
        (otherwise (push item vals))))
    (pop vals)))
(ok (arith-eval '([ 1 + [ [ 2 + 3 ] * [ 4 * 5 ] ] ] ])) 101)

"P.94"
(defun merge-sort (list comp)
  (flet ((merge-lists (l1 l2 comp)
           (let ((rez ()))
             (do ()
                 ((and (null l1) (null l2)))
               (let ((i1 (first l1))
                     (i2 (first l2)))
                 (cond ((null i1) (dolist (i l2)
                                    (push i rez))
                        (return))
                       ((null i2) (dolist (i l1)
                                    (push i rez))
                        (return))
                       ((funcall comp i1 i2) (push i1 rez)
                        (setf l1 (rest l1)))
                       (t (push i2 rez)
                          (setf l2 (rest l2))))))
             (reverse rez))))
    (if (null (rest list))
        list
        (let ((half (floor (length list) 2)))
          (merge-lists (merge-sort (subseq list 0 half) comp)
                       (merge-sort (subseq list half) comp)
                       comp)))))
(ok (merge-sort '(5 3 4 1 2) #'<) '(1 2 3 4 5))
"P.95"
;(rtl:with ((lst (random-list 10000))
;           (vec (make-array 10000 :initial-contents lst)))
;  (print-sort-timings "Prod" 'prod-sort vec)
;(print-sort-timings "Merge " 'merge-sort lst))

"P.96"
(defun generic-merge-sort (seq comp)
  (if (or (null seq)  ; avoid expensive length calculation
          (<= (length seq) 1))
      seq
      (let ((half (floor (length seq) 2)))
        (merge (type-of seq)
               (merge-sort (subseq seq 0 half) comp)
               (merge-sort (subseq seq half) comp)
               comp))))
(ok (generic-merge-sort '(5 4 3 2 1) #'<) '(1 2 3 4 5))
(type-of '(1 2))
(type-of #(1 2))
(describe #'rtl:random-list)

"P.103"
;;; alist: association list
(print '((:foo . "bar") (42 . "baz")))
;;; plist: property list
(print '(:foo "bar" 42 "baz"))

"P.110"
(defun start-memoizing (fn)
  (stop-memoizing fn)
  (setf (symbol-function fn)
        (let ((table (make-hash-table :test 'equal))
              (vanilla-fn (symbol-function fn)))
          (setf (get fn :cache) table
                (get fn :fn) vanilla-fn)
          (lambda (&rest args)
            (rtl:getsethash (format nil "~{~A~^|~}" args)
                            table
                            (apply vanilla-fn args))))))
(defun stop-memoizing (fn)
  (rtl:when-it (get fn :fn)
    (setf (symbol-function fn) rtl:it
          (get fn :fn) nil)))
(defun foo (x)
  (sleep 2)
  x)
(start-memoizing 'foo)
(time (foo 1))
(time (foo 2))
(time (foo 3))

"P.113"
(let ((i 0))
  (defun find-candidate-clock (bitmap)
    (declare (type (vector bit) bitmap))
    (loop :with len := (length bitmap)
          :until (zerop (aref bitmap i))
          :do (setf (aref bitmap i) 0)
              (setf i (mod (1+ i) len)))
    i))

"P.123"
(defun birthday-collision-prob (n)
  (let ((rez 1))
    (dotimes (i n)
      (setf rez (* rez (/ (- 365 i) 365))))
    ;; don't forget that we want the complement of the probability
    ;; of no collisions, hence (- 1.0 ...)
    (- 1.0 rez)))
(print (birthday-collision-prob 20))

(defun hash-collision-prob (n size)
  (let ((rez 1))
    (dotimes (i n)
      (setf rez (* rez (/ (- size i) size))))
    (- 1.0 rez)))
(print (hash-collision-prob 10 10))

"P.157"
