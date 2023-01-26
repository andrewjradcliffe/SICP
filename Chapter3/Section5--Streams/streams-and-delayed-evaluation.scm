;; 3.5.4 Streams and Delayed Evaluation

;; Ex. 3.77
;; y₀ + dy₀dt = y₁
;; To obtain the stream-cdr of the integrand, y₁ would be needed, but
;; it has not yet been computed.y
;; In essence, this necessitates the `delay' in the definition below.
;; It must be delayed as the stream-cdr will not be ready at the time this block
;; is evaluated -- in fact, the necessary value is computed on the line below.
;; In the implicit definition, the corresponding stream-cdr occurs within
;; a delayed block (the delayed code of stream-map), i.e.
;; (cons-stream y₁
;;              (apply stream-map (cons + (map stream-cdr dy y))))

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (delay (stream-cdr integrand))
                               (+ (* dt (stream-car integrand))
                                  initial-value)
                               dt)))))

;; Ex. 3.78

(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

;; Ex. 3.79

(define (solve-2nd f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

;; use for 3.78:
(define f-3.78 (lambda (dy y) (+ (* a dy) (* b y))))

;; Ex. 3.80

(define (RLC R L C dt)
  (define (solve-RLC vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay dIL) iL0 dt))
    (define dvC (scale-stream iL (- (/ 1 C))))
    (define diL (add-streams (scale-stream vC (/ 1 L))
                             (scale-stream iL (- (/ R L)))))
    (stream-map cons vC iL))
  solve-RLC)

(define RLC-circuit (RLC 1 1 0.2 0.1))
(define vC-iL (RLC-circuit 0 10))

(stream-ref vC-iL 10)
(stream-collect-n vC-iL 1000)
