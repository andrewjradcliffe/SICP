;; 5.4.4 Running the Evaluator

;; Ex. 5.26
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; a
#|
maximum depth is 10
|#
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)


;; b
#|
8 push per (> ...)
3 push per if
8 push per (* ...)
8 push per (+ ...)
8 push per (iter ...)


iter call, consequent branch taken: 19 (11 internal)
8 (iter ...)
3 if
8 (> ...)

iter call, alternative branch taken: 35 (27 internal)
8 (iter ...)
3 if
8 (> ...)
8 (* ...)
8 (+ ...)

n = 0, (iter 1 1) : 8 + 11
n = 1, (iter 1 1) : 8 + 27 + 8 + 11
n = 2, (iter 1 1) : 8 + 27 + 8 + 27 + 8 + 11

n + 1 (iter ...)           = 8(n + 1)
n alternative branch       = 27n
1 consequent branch        = 11

Thus, 35n + 19 from iter, and 10 from factorial body


|#
(define (iter-pushes n) (+ 19 (* 35 n)))
(define factorial-body-pushes 10)
(define (total-pushes n) (+ factorial-body-pushes (iter-pushes n)))


;; Ex. 5.27
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
;; b
#|
5 push per (factorial <literal>)
3 push per if
8 push per (= ...)
8 push per (- ...)
8 push per (* ...)

factorial call, consequent branch: 16 (11 internal)
5 (factorial ...)
3 if
8 (= ...)

factorial call, alternative branch: 32 (27 internal)
5 (factorial ...)
3 if
8 (= ...)
8 (* ...)
8 (- ...)

n = 1, (factorial 1) : 5 + 3 + 8
n = 2, (factorial 2) : 5 + 3 + 8 + 8 +  (5 + 3 + 8)  + 8
                                   ^~   ^~~~~~~~~~~    ^~
                                  (-)  (factorial 1)  (*)
n = 3, (factorial 3) : 5 + 3 + 8 + 8 + (5 + 3 + 8 + 8 +  (5 + 3 + 8)   + 8) + 8
                                   ^~               ^~   ^~~~~~~~~~~     ^~   ^~
                                  (-)              (-)  (factorial 1)   (*)  (*)
                                       ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                        (factorial 2)

n (factorial <literal>)       = 5n
n if                          = 3n
n (= ...)                     = 8n
n - 1 (- ...)                 = 8(n - 1)
n - 1 (* ...)                 = 8(n - 1)

Thus, 32n - 16 total pushes
|#
(define (total-pushes-rec n) (- (* 32 n) 16))

;; a
#|
(- n 1) : has a maximum depth of 5
(factorial <literal>) : has a maximum depth of 8 prior to reaching eval-if-decide
(factorial (- n 1)) : at the point at which (- n 1) is evaluated, stack has a depth of 3.
Thus, in order to construct the argument, a depth of 8 is reached; it then contracts to 3.

At the point one reaches (factorial (- n 1)), the stack has depth 5.

n = 1, (factorial 1) : 8
n = 2, (factorial 2) : 5 + 8
n = 3, (factorial 3) : 5 + 5 + 8

5(n - 1) + 8 = 5n + 3
|#
(define (maximum-depth-rec n) (+ 3 (* 5 n)))

#|
                        Maximum depth                 Number of pushes
Recursive factorial     5n + 3                        32n - 16
Iterative factorial     10                            35n + 19 + 10
|#


