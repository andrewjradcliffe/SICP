;; 2.1.4 Extended Exercise: Interval Arithmetic

;; Some general definitions
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;; Ex. 2.7

(define (make-interval a b) (cons a b))
(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;; Ex. 2.8

(define (negate-interval x)
  (let ((p1 (- (lower-bound x)))
        (p2 (- (upper-bound x))))
    (make-interval (min p1 p2)
                   (max p1 p2))))

(define (sub-interval x y)
  (add-interval x (negate-interval y)))


;; Ex. 2.9
;; See p. 1 for proof.

(define (width-interval x)
  (- (upper-bound x) (lower-bound x)))


;; Ex. 2.10

(define (inv-interval x)
  (let ((p1 (lower-bound x))
        (p2 (upper-bound x)))
    (if (or (= 0 p1) (= 0 p2))
        (error "Attempt to divide by interval which has lower or upper bound of zero" x)
        (make-interval (/ 1.0 p2) (/ 1.0 p1)))))

(define (div-interval x y)
  (mul-interval x (inv-interval y)))


;; Ex. 2.11

(define (mul-interval x y)
  (let ((a (lower-bound x))
        (b (upper-bound x))
        (c (lower-bound y))
        (d (upper-bound y)))
    (cond ((and (>= a 0) (>= b 0) (>= c 0) (>= d 0))
           (make-interval (* a c) (* b d)))
          ((and (< a 0) (>= b 0) (>= c 0) (>= d 0))
           (make-interval (* a d) (* b d)))
          ((and (< a 0) (< b 0) (>= c 0) (>= d 0))
           (make-interval (* a d) (* b c)))
          ((and (< a 0) (< b 0) (< c 0) (>= d 0))
           (make-interval (* a d) (* a c)))
          ((and (< a 0) (< b 0) (< c 0) (< d 0))
           (make-interval (* b d) (* a c)))
          ((and (< a 0) (>= b 0) (< c 0) (< d 0))
           (make-interval (* b c) (* a c)))
          ((and (< a 0) (>= b 0) (< c 0) (>= d 0))
           (make-interval (min (* a d) (* b c))
                          (max (* a c) (* b d))))
          ((and (>= a 0) (>= b 0) (< c 0) (>= d 0))
           (make-interval (* b c) (* b d)))
          ((and (>= a 0) (>= b 0) (< c 0) (< d 0))
           (make-interval (* b c) (* a d))))))


;; Ex. 2.12

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-center-width c (* c p 0.01)))

(define (percent i)
  (let ((a (lower-bound i))
        (b (upper-bound i)))
    (* 100 (/ (- b a) (+ a b)))))
