;; 1.2.4 Exponentiation

;; For comparison
(define (expt-iter b n)
  (define (iter b n product)
    (if (= n 0)
        product
        ((expt-iter b (- n 1) (* b product)))))
  (iter b n 1))
(define (expt-rec b n)
  (if (= n 0)
      1
      ((* b (expt b (- n 1))))))
(define (fast-expt b n)
  (cond ((= n 0)
         1)
        ((even? n)
         (square (fast-expt b (/ n 2))))
        (else
         (* b (fast-expt b (- n 1))))))

;; Ex. 1.16
(define (fast-expt-iter b n)
  (define (iter a b n)
    (cond ((= 0 n) a)
          ((even? n)
           (iter a (* b b) (/ n 2)))
          (else
           (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;; Ex. 1.17
(define (double x) (* x 2))
(define (halve x) (/ x 2))
(define (*-rec a b)
  (cond ((= b 0) 0)
        ((even? b)
         (double (*-rec a (halve b))))
        (else
         (+ a (* a (- b 1))))))

;; Ex. 1.18
(define (*-iter a b)
  (define (iter s a b)
    (cond ((= 0 b) s)
          ((even? b)
           (iter s (double a) (halve b)))
          (else
           (iter (+ s a) a (- b 1)))))
  (iter 0 a b))

;; Ex. 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q n)
  (cond ((= n 0) b)
        ((even? n)
         (fib-iter
          a
          b
          (+ (* q q) (* p p))
          (+ (* q q) (* 2 p q))
          (/ n 2)))
        (else
         (fib-iter
          (+ (* b q) (* a q) (* a p))
          (+ (* b p) (* a q))
          p
          q
          (- n 1)))))

#|
In fact, we can use the matrix exponential form to express negative Fibonacci numbers.
|#

(define (fib-both n)
  (define (fib-iter a b p q n)
    (cond ((= n 0) b)
          ((even? n)
           (fib-iter
            a
            b
            (+ (* q q) (* p p))
            (+ (* q q) (* 2 p q))
            (/ n 2)))
          (else
           (fib-iter
            (+ (* b q) (* a q) (* a p))
            (+ (* b p) (* a q))
            p
            q
            (- n 1)))))
  (define (fib-iter-neg a b p q n)
    (cond ((= n 0) b)
          ((even? n)
           (fib-iter-neg
            a
            b
            (+ (* p p) (* q q))
            (- (* 2 p q) (* q q))
            (/ n 2)))
          (else
           (fib-iter-neg
            (+ (* a p) (* b q))
            (+ (* b p) (* a q) (- (* b q)))
            p
            q
            (+ n 1)))))
  (if (>= n 0)
      (fib-iter 1 0 0 1 n)
      (fib-iter-neg 1 0 0 1 n)))
