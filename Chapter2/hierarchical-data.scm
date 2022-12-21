;; 2.2.1 Representing Sequences

(define (my-append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (copy x)
  (if (null? x)
      '()
      (cons (car x) (copy (cdr x)))))

;; Ex. 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (list (car items))
      (last-pair (cdr items))))

;; Ex. 2.18
(define (my-reverse items)
  (reverse-impl items '()))

(define (reverse-impl list1 list2)
  (if (null? list1)
      list2
      (reverse-impl (cdr list1) (cons (car list1) list2))))

;; (= (my-reverse (list 1 2 3)) (reverse (list 1 2 3)))


;; Ex. 2.20
(define (same-parity x . y)
  (let ((p? (if (even? x) even? odd?))
        (z (list x)))
    (reverse (same-parity-rec p? y z))))

(define (same-parity-iter p? y z)
  (if (null? y)
      z
      (if (p? (car y))
          (same-parity-iter p? (cdr y) (cons (car z) (cons (car y) (cdr z))))
          ;; (same-parity-iter p? (cdr y) (cons (cons (car z) (car y)) (cdr z)))
          ;; (same-parity-iter p? (cdr y) (cons (car z) (cons (cdr z) (car y))))
          (same-parity-iter p? (cdr y) z))))

(define (same-parity-rec p? y z)
  (if (null? y)
      z
      (if (p? (car y))
          (same-parity-rec p? (cdr y) (cons (car y) z))
          ;; (same-parity-rec p? (cdr y) (append (list (car y)) z))
          ;; (same-parity-rec p? (cdr y) (cons (car z) (car y)))
          (same-parity-rec p? (cdr y) z))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

;; Revised!
(define (same-parity-2 x . y)
  (let ((p? (if (even? x) even? odd?)))
    (cons x (same-parity-impl-2 p? y))))

(define (same-parity-impl-2 p? y)
  (if (null? y)
      '()
      (if (p? (car y))
          (cons (car y) (same-parity-impl-2 p? (cdr y)))
          (same-parity-impl-2 p? (cdr y)))))

;; 2.2.2. Hierarchical Data

;; Ex. 2.24
(define l3 (list 1 (list 2 (list 3 4))))

(= (cadr (cadr (cadr l3))) 4)

;; Ex. 2.25
(define l225_1 (list 1 3 (list 5 7) 9))
(= (car (cdr (car (cdr (cdr l225_1))))) 7)
(= (cadr (cadr (cdr l225_1))) 7)

(define l225_2 (list (list 7)))
(= (car (car l225_2)) 7)
(= (caar l225_2) 7)

(define l225_3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(= (cadr (cadr (cadr (cadr (cadr (cadr l225_3)))))) 7)

;; Ex 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; (1 2 3 4 5 6)
(cons x y)   ; ((1 2 3) 4 5 6)
(list x y)   ; ((1 2 3) (4 5 6))

;; Ex 2.28
(define l228 (list (list 1 2) (list 3 4)))

(define (fringe x)
  (fringe-impl x '()))

(define (fringe-impl x y)
  (cond ((null? x) y)
        ((not (pair? x))
         (cons x y))
        ;; (else (fringe-impl (cdr x) (fringe-impl (car x) y)))
        (else (fringe-impl (car x) (fringe-impl (cdr x) y)))
        ))

(fringe l228)
(fringe (list l228 l228))
