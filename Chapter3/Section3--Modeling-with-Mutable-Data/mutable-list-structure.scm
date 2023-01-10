;; 3.3.1 Mutable List Structure

;; Ex. 3.12

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
(cdr x) ; '(b)
(define w (append! x y))
(cdr x) ; '(b c d)


;; Ex. 3.13

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))
;; (last-pair z) results in an infinite loop as a null cdr will never be found,
;; as upon reading the previous last-pair (c), the cdr pointer simply points to
;; the beginning.


;; Ex. 3.14

;; mystery reverses the order of the elements by storing the remaining pairs
;; in a temporary, then setting the cdr of the first pair to the accumulated elements.


(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v '(a b c d))
(define w (mystery v))
;; w := (d c b a); v := (a)


;; Ex. 3.15


;; Ex. 3.16

(define (count-pairs-1 x)
  (if (not (pair? x))
      0
      (+ (count-pairs-1 (car x))
         (count-pairs-1 (cdr x))
         1)))

;; never return:
(define nr (make-cycle '(a b c)))

;; return 3:
(define r3 '(a b c))

;; return 4
(define r4 '(a b c))
(set-car! r4 (last-pair r4))

;; return 7
(define r7 '(a b c))
(set-car! r7 (cdr r7))
(set-car! (cdr r7) (cddr r7))



;; Ex. 3.17
(define (any-eq? x L)
  (if (null? L)
      false
      (or (eq? x (car L)) (any-eq? x (cdr L)))))

(define (count-pairs-2 x)
  (define (iter x count aux)
    (if (pair? x)
        (if (any-eq? x aux)
            count
            (begin
              ;; (append! aux (list x))
              (if (null? aux) (set! aux (list x)) (append! aux (list x)))
                   (+ (iter (car x) 0 aux)
                      (iter (cdr x) 0 aux)
                      1)))
        0))
  (iter x 0 '()))

(define (append!! x y)
  (if (null? x)
      (begin (set! x y)
             x)
      (append! x y)))

(count-pairs-2 nr)
(count-pairs-2 r3)
(count-pairs-2 r4)
(count-pairs-2 r7)

(define x '(a b))
(define y (cons x x))
(define z (cons y y))

(count-pairs-2 x)
(count-pairs-2 y)
(count-pairs-2 z)
