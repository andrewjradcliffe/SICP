;; 2.2.1 Representing Sequences

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
    (same-parity-rec p? y z)))

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

;; 2.2.2. Hierarchical Data

;; Ex. 2.24
(define l3 (list 1 (list 2 (list 3 4))))

(= (cadr (cadr (cadr l3))) 4)

;; Ex. 2.25

