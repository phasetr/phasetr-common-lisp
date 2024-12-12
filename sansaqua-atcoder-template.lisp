;; -*- coding: utf-8 -*-
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-int:defconstant-eqx OPT
    #+swank '(optimize (speed 3) (safety 2))
    #-swank '(optimize (speed 3) (safety 0) (debug 0))
    #'equal)
  #+swank (ql:quickload '(:cl-debug-print :fiveam) :silent t)
  #-swank (set-dispatch-macro-character
           #\# #\> (lambda (s c p) (declare (ignore c p)) (read s nil (values) t))))
#+swank (cl-syntax:use-syntax cl-debug-print:debug-print-syntax)
#-swank (disable-debugger) ; for CS Academy

;; BEGIN_INSERTED_CONTENTS
;; clojure-like placehoder syntax
(defun placeholder-p (symbol)
  (let ((name (symbol-name symbol)))
    (and (>= (length name) 1)
         (char= (char name 0) #\%)
         (loop for i from 1 below (length name)
               always (digit-char-p (char name i))))))

(defun get-place-number (symbol)
  (let* ((name (symbol-name symbol))
         (len (length name)))
    (if (= len 1)
        1
        (parse-integer name :start 1))))

(defun parse-placeholder-form (form)
  (let ((arity 0)
        (args (make-array 1 :fill-pointer 0)))
    (labels ((add-arg! (pos)
               (loop for i from arity below pos
                     do (vector-push-extend (gensym) args))
               (setq arity (max arity pos)))
             (recur (x)
               (cond ((and (symbolp x)
                           (placeholder-p x))
                      (let ((pos (get-place-number x))) ; 1-based
                        (add-arg! pos)
                        (aref args (- pos 1))))
                     ((consp x)
                      (mapcar #'recur x))
                     (t x))))
      (let ((body (recur form)))
        `(lambda ,(coerce args 'list)
           ,body)))))

(defun read-placeholder (s c p)
  (declare (ignore c p))
  (let ((form (read s nil nil t)))
    (parse-placeholder-form form)))

;; (cl-syntax:defsyntax placeholder-syntax
;;   (:merge :standard)
;;   (:dispatch-macro-char #\# #\% #'read-placeholder))

;; (cl-syntax:use-syntax placeholder-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (set-dispatch-macro-character #\# #\% #'read-placeholder))

;; unfinished

(declaim (inline map-monotone-subseq))
(defun map-monotone-subseq (function seq order)
  "Applies FUNCTION to each monotone subsequence (or substring, strictly
speaking) of SEQ. FUNCTION must take two arguments, L and R, where the interval
[L, R) corresponds to a monotone subsequence.

SEQ[l, r) is monotone iff (FUNCALL ORDER SEQ[i] SEQ[i+1]) always returns
true in this range."
  (declare (sequence seq)
           (function function order))
  (etypecase seq
    (vector
     (unless (zerop (length seq))
       (let ((base 0))
         (loop for i from 1 below (length seq)
               unless (funcall order (aref seq (- i 1)) (aref seq i))
                 do (funcall function base i)
                    (setq base i)
               finally (funcall function base (length seq))))))
    (list
     "Not implemented yet.")))

(declaim (ftype (function * (values fixnum &optional)) read-fixnum))
(defun read-fixnum (&optional (in *standard-input*))
  (declare #.OPT)
  (macrolet ((%read-byte ()
               `(the (unsigned-byte 8)
                     #+swank (char-code (read-char in nil #\Nul))
                     #-swank (sb-impl::ansi-stream-read-byte in nil #.(char-code #\Nul) nil))))
    (let* ((minus nil)
           (result (loop (let ((byte (%read-byte)))
                           (cond ((<= 48 byte 57)
                                  (return (- byte 48)))
                                 ((zerop byte) ; #\Nul
                                  (error "Read EOF or #\Nul."))
                                 ((= byte #.(char-code #\-))
                                  (setf minus t)))))))
      (declare ((integer 0 #.most-positive-fixnum) result))
      (loop
        (let* ((byte (%read-byte)))
          (if (<= 48 byte 57)
              (setq result (+ (- byte 48)
                              (* 10 (the (integer 0 #.(floor most-positive-fixnum 10)) result))))
              (return (if minus (- result) result))))))))

(defmacro dbg (&rest forms)
  #+swank
  (if (= (length forms) 1)
      `(format *error-output* "~A => ~A~%" ',(car forms) ,(car forms))
      `(format *error-output* "~A => ~A~%" ',forms `(,,@forms)))
  #-swank (declare (ignore forms)))

(defmacro define-int-types (&rest bits)
  `(progn
     ,@(mapcar (lambda (b) `(deftype ,(intern (format nil "UINT~A" b)) () '(unsigned-byte ,b))) bits)
     ,@(mapcar (lambda (b) `(deftype ,(intern (format nil "INT~A" b)) () '(signed-byte ,b))) bits)))
(define-int-types 2 4 7 8 15 16 31 32 62 63 64)

(declaim (inline println))
(defun println (obj &optional (stream *standard-output*))
  (let ((*read-default-float-format* 'double-float))
    (prog1 (princ obj stream) (terpri stream))))

(defconstant +mod+ 1000000007)

;;;
;;; Body
;;;

#-swank (main)
