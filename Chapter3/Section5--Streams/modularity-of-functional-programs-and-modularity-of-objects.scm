;; 3.5.5 Modularity of Functional Programs and Modularity of Objects

;; Simplest custom RNG: congruential with period 2^32
(define (rand-update x)
  ;; a 69
  ;; k 13
  ;; m 1024, or, 2^32 4294967296, or 2^64 18446744073709551616
  (remainder (+ (* 69 x) 13) 4294967296))

(define random-numbers
  (cons-stream 34875799 ;;308
               (stream-map rand-update random-numbers)))

(stream-collect-n random-numbers 10)

(define u-01-random-numbers
  (scale-stream random-numbers (/ 1.0 4294967296)))

(/ (fold-left + 0 (stream-collect-n u-01-random-numbers 10000)) 10000)

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

(stream-ref pi 100000)

;; Ex. 3.81

(define (make-rand random-init)
  (define random-numbers
    (cons-stream random-init
                 (stream-map rand-update random-numbers)))
  (define (rand-iter rest request-stream)
    (if (eq? (stream-car request-stream) 'generate)
        (cons-stream (stream-car rest)
                     (rand-iter (stream-cdr rest) (stream-cdr request-stream)))
        (cons-stream (stream-car random-numbers)
                     (rand-iter random-numbers (stream-cdr request-stream)))))
  (define (rand request-stream)
    (rand-iter random-numbers request-stream))
  rand)

(define rand (make-rand 308))
(define requests-g (cons-stream 'generate requests-g))
(define requests-r (cons-stream 'reset requests-r))
(define requests (interleave requests-g (interleave requests-g requests-r)))
(define nums (rand requests))

(stream-collect-n nums 10)

;; Ex. 3.82

(define (uniform-random a b u)
  (+ a (* u (- b a))))

(define (experiment-stream pred x1 x2 y1 y2 u-01-random-numbers)
  (map-successive-pairs
   (lambda (u1 u2) (pred (uniform-random x1 x2 u1) (uniform-random y1 y2 u2)))
   u-01-random-numbers))

(define (stream-estimate-integral pred x1 x2 y1 y2 u-01-random-numbers)
  (scale-stream
   (monte-carlo (experiment-stream pred x1 x2 y1 y2 u-01-random-numbers) 0 0)
   (* (- x2 x1) (- y2 y1))))

(define (within-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

(define pi-stream
  (stream-estimate-integral within-unit-circle? -1.0 1.0 -1.0 1.0 u-01-random-numbers))

(stream-ref pi-stream 10000)
