;; 3.1.2 The Benefits of Introducing Assignment

;; Ex. 3.5
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (make-circle-predicate x0 y0 r)
  (lambda (x y) (<= (+ (square (- x x0))
                       (square (- y y0)))
                    (square r))))

(define indicator-pi (make-circle-predicate 0 0 1))

(define (within-unit-circle? x y)
  (<= (+ (square x) (square y)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-integral pred x1 x2 y1 y2 trials)
  (define (experiment)
    (pred (random-in-range x1 x2) (random-in-range y1 y2)))
  (* (- x2 x1) (- y2 y1) (monte-carlo trials experiment)))

(define (pi-mc trials)
  (estimate-integral within-unit-circle? -1.0 1.0 -1.0 1.0 trials))
(define (pi-mc-2 trials)
  (estimate-integral indicator-pi -1.0 1.0 -1.0 1.0 trials))

(pi-mc 10000)
(pi-mc-2 10000)


;; Ex. 3.6
;; a 427419669081
;; b 1
;; c 999999999989
(define (rand-update x)
  (define a 427419669081)
  (define c 999999999989)
  (remainder (* a x) c))

(define (make-rand x)
  (define (generate)
    (begin (set! x (rand-update x))
           x))
  (define (reset y)
    (begin (set! x y) x))
  (define (dispatch m)
    (cond ((eq? m 'generate) (generate))
          ((eq? m 'reset) reset)
          (else (error "Unknown method -- MAKE-RAND" m))))
  dispatch)

(define rand (make-rand 123456789))

(rand 'generate)
((rand 'reset) 123456789)
(rand 'generate)

;; This fails because it simply returns a new object initialized at the new state.
(define (make-rand-bad random-init)
  (let ((x random-init))
    (lambda (m)
      (cond ((eq? m 'generate)
             (set! x (rand-update x))
             x)
            ((eq? m 'reset) make-rand-bad)
            (else (error "Unknown method -- MAKE-RAND-BAD" m))))))

(define rand-bad (make-rand-bad 1234))
(rand-bad 'generate)
((rand-bad 'reset) 1234)
(rand-bad 'generate)
