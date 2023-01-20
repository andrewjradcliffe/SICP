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
