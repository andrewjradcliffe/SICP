;; 3.5.1 Streams Are Delayed Lists

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

;; Ex. 3.50

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;; Ex. 3.51

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
;; Prints 0 as first call of stream-map results in a cons-stream call,
;; the car of which is evaluated. Evaluation of the car corresponds to a call
;; of show on the stream-car of the stream produced by (stream-enumerate-interval 0 10).
;; The cons-stream inside that stream contains 0. Hence, 0 is printed and is also available
;; by calling stream-car on x.

(stream-ref x 5)
;; prints 1,2,3,4,5, then returns 5

(stream-ref x 7)
;; prints 6,7, then returns 7
;; If delay is defined using memo-proc, then only 6,7 should be printed as
;; the body of stream-map has already been evaluated for 1,2,3,4,5.
;; (technically, the force of delays with the same expressions appear for the first 5 iterations)


;; Ex. 3.52

;; Several variations, with amusing results based on return value of accum.

;; First, the expected result given memoization of delayed calls and correct definition
;; of accum.

(define sum 0) ; 0

(define (accum x) ; 0
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20))) ; 1

(define y (stream-filter even? seq)) ; 3, 6

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) ; 10
                         seq))

(stream-ref y 7) ; returns 136, and sum is now 136

(display-stream z) ; prints 10,15,45,55,105,120,190,210, and sum is now 210

;; With memo-proc, the stream-map calls occur only once per argument, hence,
;; each value of (stream-enumerate-interval 1 20) is added to sum only once.
;; This behavior differs considerably from the behavior without memo-proc.
;; Incidentally, with memo-proc, the results of the last 3 expressions should be
;; the same regardless of the number of calls -- without memo-proc they will
;; change each time. Even the stream-ref and display-stream calls would change each time
;; without memo-proc.

;; See p. 90 and 91 for the equivalent exercise performed without memo-proc.


;; Second, the expected result given memoization of delayed calls and the incorrect
;; definition of accum.

(define sum 0) ; 0

(define (accum x) ; 0
  (set! sum (+ x sum))
  x)

(define seq (stream-map accum (stream-enumerate-interval 1 20))) ; 1

(define y (stream-filter even? seq)) ; 1 + 2 = 3

(define z (stream-filter (lambda (x) (= (remainder x 5) 0)) ; (1 + 2) + 3 + 4 + 5 = 15
                         seq))

(stream-ref seq 7) ; 15 + 6 + 7 + 8 = 36

(stream-ref y 7) ; 15 + (6 + ... + 16) = 136, returns 16

(display-stream z) ; 136 + 17 + 18 + 19 + 20 = 210, prints 5, 10, 15, 20
