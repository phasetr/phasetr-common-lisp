(ql:quickload :rove)
(use-package :rove)

(testing "P.28, place to quote 1"
  (flet ((exclaim1 (expression)
           (append expression '(oh my))))
    (ok '(exclaim1 '(lions and tigers and bears))
        '(LIONS AND TIGERS AND BEARS OH MY))
    (ok '(nconc * '(goodness))
        '(LIONS AND TIGERS AND BEARS OH MY GOODNESS))
    (ok '(exclaim1 '(fixnums and bignums and floats))
        '(FIXNUMS AND BIGNUMS AND FLOATS OH MY GOODNESS))))

(testing "P.28, place to quote 2"
  (flet ((exclaim2 (expression)
           (append expression (list 'oh 'my))))
    (ok '(exclaim2 '(lions and tigers and bears))
        '(LIONS AND TIGERS AND BEARS OH MY))
    (ok '(nconc * '(goodness))
        '(LIONS AND TIGERS AND BEARS OH MY GOODNESS))
    (ok '(exclaim2 '(fixnums and bignums and floats))
        '(FIXNUMS AND BIGNUMS AND FLOATS OH MY))))
