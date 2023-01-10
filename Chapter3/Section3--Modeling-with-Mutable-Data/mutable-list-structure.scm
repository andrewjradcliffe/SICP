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


;;                    +----------------------------
;;                    |                           |
;;                    |    +-------++-------+     |
;;                    |    |       ||       |     |
;;                    +--->|   O   ||   O---+-----+
;;                         |   |   ||       |
;;                         +---+---++---+---+
;;                             |        ^
;;                             |        |
;;                             +--------+
(define ww '(1 . 2))
(set-car! ww ww)
(set-cdr! ww ww)

;;               +----------------------------------------------------------------------------+
;;               |                                                                            |
;;               |                                                                            |
;;               v                                                                            |
;;           +-------++-------+                +-------++-------+                +-------++---+---+
;;           |       ||       |                |       ||       |                |       ||   |   |
;;    +----->|   O   ||   O---+--------------->|   O   ||   O---+--------------->|   O   ||   O   |
;;    |      |   |   ||       |                |   |   ||       |                |   |   ||       |
;;    |      +---+---++-------+                +---+---++-------+                +---+---++-------+
;;    |          |                                 |                                 |
;;    |          |                                 |                                 |
;;    |          v                                 |                                 v
;;    |          a                                 |                                 c
;;    |                                            |
;;    |                                            |
;;    +--------------------------------------------+

(define www '(a b c)) ; just the bottom
(set-car! (cdr www) www)
(define www-both '(a b c)) ; both top and bottom
(set-car! (cdr www-both) www-both)
(set-cdr! (last-pair www-both) www-both)


;;               +----------------------------------------------------------------------------+
;;               |                                                                            |
;;               |                                                                            |
;;               v                                                                            |
;;           +-------++-------+                +-------++-------+                +-------++---+---+
;;           |       ||       |                |       ||       |                |       ||   |   |
;;    +----->|   O   ||   O---+--------------->|   O   ||   O---+--------------->|   O   ||   O   |
;;    |      |   |   ||       |                |   |   ||       |         +----->|   |   ||       |
;;    |      +---+---++-------+                +---+---++-------+         |      +---+---++-------+
;;    |          |                                 |                      |          |
;;    |          |                                 |                      |          |
;;    |          v                                 +----------------------+          |
;;    |          a                                                                   |
;;    |                                                                              |
;;    |                                                                              |
;;    +-------------------------------------------------------------------------------

(define wwww '(a b c)) ; just the bottom
(set-car! (cdr wwww) (cddr wwww))
(set-car! (cddr wwww) wwww)

(define wwww-both '(a b c)) ; both top and bottom
(set-car! (cdr wwww) (cddr wwww))
(set-car! (cddr wwww) wwww)
(set-cdr! (cddr wwww) wwww)

;; Ex. 3.18

(define (has-cycle? x)
  (define (iter x aux)
    (if (pair? x)
        (if (any-eq? x aux)
            (has-cycle-simple? x)
            (begin
              (if (null? aux) (set! aux (list x)) (append! aux (list x)))
              (or (iter (car x) aux) (iter (cdr x) aux))))
        false))
  (define (has-cycle-simple? x)
    (define (iter-simple x aux)
      (if (pair? x)
          (if (any-eq? x aux)
              true
              (begin
                (if (null? aux) (set! aux (list x)) (append! aux (list x)))
                (or (iter-simple (car x) aux) (iter-simple (cdr x) aux))))
          false))
    (iter-simple x '()))
  (iter x '()))

(define (has-cycle-2? x)
  (define (iter x aux)
    (if (pair? x)
        (if (any-eq? x aux)
            true
            (begin
              (if (null? aux) (set! aux (list x)) (append! aux (list x)))
              (or (iter (car x) (list x)) (iter (cdr x) aux))))
        false))
  (iter x '()))


(has-cycle-2? x)
(has-cycle-2? y)
(has-cycle-2? z)

(has-cycle-2? ww)
(has-cycle-2? www)
(has-cycle-2? nr)
(has-cycle-2? r3)
(has-cycle-2? r4)
(has-cycle-2? r7)

