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


