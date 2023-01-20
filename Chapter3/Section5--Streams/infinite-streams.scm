;; 3.5.2 Infinite Streams

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define integers (integers-starting-from 1))

(define ones (cons-stream 1 ones))

(define (stream-null? s) (null? s))
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (negate-stream s) (scale-stream s -1))

(define (sub-streams s1 s2) (add-stream s1 (negate-stream s2)))

;; not very useful on infinite streams
(define (stream-collect s)
  (if (stream-null? s)
      '()
      (cons (stream-car s)
            (stream-collect (stream-cdr s)))))

;; more practical
(define (stream-collect-n s n)
  (define (iter s m)
    (if (> m n)
        '()
        (cons (stream-car s)
              (iter (stream-cdr s) (+ m 1)))))
  (iter s 0))

;; Ex. 3.53
;; The elements are 1,2,4,8,16,... with the next element twice the last.
;; Specifically, the elements are powers of 2, when starting from 0, or
;; m * 2^i where i is the index in the stream and m is some pre-factor.
;; See note on p.94 for illustrative figure.
(define s (cons-stream 1 (add-streams s s)))


;; Ex. 3.54

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

;; this gives n factorial as nth element
(define factorials (cons-stream 1 (mul-streams factorials integers)))
;; gives n+1 factorial as nth element
(define factorials (cons-stream 1 (mul-streams factorials (stream-cdr integers))))

;; Ex. 3.55

;; This depends on memo-proc being used to define delay, i.e. call-by-need.
;; This is not ideal -- see the implementation below.
(define (partial-sums-mut s)
  (let ((sum 0))
    (define (accum x)
      (set! sum (+ x sum))
      sum)
    (stream-map accum s)))

;; The preferred implementation
(define (partial-sums s)
  (define (iter sum s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (+ sum (stream-car s))
                     (iter (+ sum (stream-car s)) (stream-cdr s)))))
  (iter 0 s))

;; Ex. 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 ((= s1car s2car)
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

(stream-car (stream-cdr S))

;; Ex. 3.57
;; See p. 95 for diagram and result.

;; Ex. 3.58
;; Seemingly, these are the stream of coefficients produced by long division,
;; at least for radix=10.

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(stream-collect-n (expand 1 7 10) 10)
(stream-collect-n (expand 3 8 10) 10)

(stream-collect-n (expand 1 7 2) 10)
(stream-collect-n (expand 3 8 2) 10)

;; Ex. 3.59

;; a

(define (integrate-series s)
  (stream-map / s integers))

;; b

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (integrate-series (negate-stream sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(define (partial-products s)
  (define (iter product s)
    (if (stream-null? s)
        the-empty-stream
        (cons-stream (* product (stream-car s))
                     (iter (* product (stream-car s)) (stream-cdr s)))))
  (iter 1 s))

(define (streaming-expt x)
  (partial-products (cons-stream 1 (scale-stream ones x))))

(define (power-series s x)
  (mul-streams s (streaming-expt x)))

(define (streaming-exp x)
  (partial-sums (power-series exp-series x)))
(define (streaming-cosine x)
  (partial-sums (power-series cosine-series x)))
(define (streaming-sine x)
  (partial-sums (power-series sine-series x)))

;; Ex. 3.60

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (mul-series (stream-cdr s1) s2)
                            (mul-series s1 (stream-cdr s2)))))
;; Pascal's triangle
(stream-collect-n (mul-series ones ones) 10)
;;                1
;;               1 1
;;              1 2 1
;;             1 3 3 1
;;            1 4 6 4 1
;;               ...
;;
;; Clearly, this mul-series is wrong.

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                            (mul-series s1 (stream-cdr s2)))))

(define (streaming-sin-squared x)
  (partial-sums (power-series (mul-series sine-series sine-series) x)))

(stream-ref (streaming-sin-squared 0.5) 30)

(define (sine-squared x) (* (sin x) (sin x)))
