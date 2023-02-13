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

#|
Ex. 4.35's version searches                i=low,...,high
                                           j=i,...,high
                                           k=j,...,high

We can illustrate with high=4

i = 1
                    k
    | (1,1) | (1,2) | (1,3) | (1,4) |
j   |       | (2,2) | (2,3) | (2,4) |
    |       |       | (3,3) | (3,4) |
    |       |       |       | (4,4) |


i = 2
                    k
    |       |       |       |       |
j   |       | (2,2) | (2,3) | (2,4) |
    |       |       | (3,3) | (3,4) |
    |       |       |       | (4,4) |


i = 3
                    k
    |       |       |       |       |
j   |       |       |       |       |
    |       |       | (3,3) | (3,4) |
    |       |       |       | (4,4) |


i = 4
                    k
    |       |       |       |       |
j   |       |       |       |       |
    |       |       |       |       |
    |       |       |       | (4,4) |


With i = 1, we see the analogy to an upper triangular matrix,
which has:
n * (n + 1) / 2                 non-empty elements
n = high - i + 1                which we can write as n(i) = high - i + 1

Thus, one searches the number of possibilities:
    ∑ᵢ₌₁ʰⁱᵍʰ n(i) * (n(i) + 1) / 2
= ∑ᵢ₌₁ʰⁱᵍʰ (high - i + 1) * (high - i + 1 + 1) / 2
= (1/2) ∑ᵢ₌₁ʰⁱᵍʰ (high - i + 1) * (high - i + 2)

Expanding:
(high - i + 1) * (high - i + 2) = high^2 - 2i * high + high + i^2 - 3i + 2

Thus, each term is 𝒪(high^2)
Therefore, a sum of high terms is 𝒪(high^3)




Ex. 4.37's version searches                i=low,...,high
                                           j=i,...,high
                                           high^2 >= k^2
                                           k^2 = i^2 + j^2

It searches the grid:
                    j
    | (1,1) | (1,2) | (1,3) | (1,4) |
i   |       | (2,2) | (2,3) | (2,4) |
    |       |       | (3,3) | (3,4) |
    |       |       |       | (4,4) |

Even if we assume that each (i, j) proposal incurs the full cost of both
require's (n.b. the sqrt is often skipped), then the total number of points is:
n * (n + 1) / 2                non-empty elements
n = high

Thus, high * (high + 1) / 2 elements, which is 𝒪(high^2)



---
Ben is certainly correct.

If we consider the cost of the individual operations -- mul, add, sqrt --
then the prefactors on the order of complexity will display differences, but
it is unlikely that we could find a value of high such taht Ex. 4.35's version
is faster than Ex. 4.37's version.

|#
