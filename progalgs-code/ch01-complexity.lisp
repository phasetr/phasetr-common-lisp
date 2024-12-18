(in-package :progalgs)

(defun mat-max (mat)
  (let (max)
    (dotimes (i (array-dimension mat 0))
      (dotimes (j (array-dimension mat 1))
        (when (or (null max)
                  (> (aref mat i j) max))
          (setf max (aref mat i j)))))
    max))

(mat-max #2A((1 2 3) (4 5 6)))

(deftest mat-max ()
  (should be null (mat-max #2A()))
  (should be = 42 (mat-max #2A((42))))
  (should be = 6 (mat-max #2A((1 2 3) (4 5 6)))))
