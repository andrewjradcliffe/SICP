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
