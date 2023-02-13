;; 4.3.1 Amb and Search

;; Ex. 4.35

(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

;; Ex. 4.36
#|
Consider the behavior of a-pythagorean-triangle-between : backtracking occurs
from the most recent choice point, which is select of k.

The first try will be i,j,k = 1,1,1, which fails. Then, we backtrack to
selection of k, and fail on k=2,...,high. Thus, we backtrack to selection of j.
Then, for j=2,...,high, we try all values of k=2,...,high.
Let us suppose that high is not infinity, so that we are forced to backtrack to i.
We then try i=2 and proceed through j and k. This is feasible because there is
an upper bound on the choices, which causes the branching on k (and j) to eventually
stop, thereby initiating a backtracking to the previous choice point.

If one used an-integer-starting-from, then the branching on the k choice point
would never end. In essence, the algorithm would be attempting values of k with
i,j = 1,1 that are guaranteed to fail, forever.

Solution

i <= j <= k must be true, thus, we could think of this as selecting a k, then
                          selecting a j, then an i.

|#
(define (pythagorean-triples)
  (let ((k (an-integer-starting-from 1)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))



;; 4.37

