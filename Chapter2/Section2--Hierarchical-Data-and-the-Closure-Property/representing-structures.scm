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
(define (my-last-pair items)
  (if (null? (cdr items))
      (list (car items))
      (my-last-pair (cdr items))))

;; Ex. 2.18
(define (my-reverse items)
  (reverse-impl items '()))

(define (reverse-impl list1 list2)
  (if (null? list1)
      list2
      (reverse-impl (cdr list1) (cons (car list1) list2))))

;; (= (my-reverse (list 1 2 3)) (reverse (list 1 2 3)))


;; Ex. 2.20
;; (define (same-parity x . y)
;;   (let ((p? (if (even? x) even? odd?))
;;         (z (list x)))
;;     (reverse (same-parity-rec p? y z))))

;; (define (same-parity-iter p? y z)
;;   (if (null? y)
;;       z
;;       (if (p? (car y))
;;           (same-parity-iter p? (cdr y) (cons (car z) (cons (car y) (cdr z))))
;;           ;; (same-parity-iter p? (cdr y) (cons (cons (car z) (car y)) (cdr z)))
;;           ;; (same-parity-iter p? (cdr y) (cons (car z) (cons (cdr z) (car y))))
;;           (same-parity-iter p? (cdr y) z))))

;; (define (same-parity-rec p? y z)
;;   (if (null? y)
;;       z
;;       (if (p? (car y))
;;           (same-parity-rec p? (cdr y) (cons (car y) z))
;;           ;; (same-parity-rec p? (cdr y) (append (list (car y)) z))
;;           ;; (same-parity-rec p? (cdr y) (cons (car z) (car y)))
;;           (same-parity-rec p? (cdr y) z))))

;; Revised!
(define (same-parity x . y)
  (let ((p? (if (even? x) even? odd?)))
    (cons x (same-parity-impl p? y))))

(define (same-parity-impl p? y)
  (if (null? y)
      '()
      (if (p? (car y))
          (cons (car y) (same-parity-impl p? (cdr y)))
          (same-parity-impl p? (cdr y)))))

(equal? (same-parity 1 2 3 4 5 6 7) (list 1 3 5 7))
(equal? (same-parity 2 3 4 5 6 7) (list 2 4 6))

;; Ex. 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;; Ex 2.23
;; (define (my-for-each proc x)
;;   (if (null? x)
;;       true
;;       ((proc (car x))
;;        (my-for-each proc (cdr x))
;;        true)))

(define (my-for-each proc x)
  (map (lambda (z) (proc z)) x) true)

(my-for-each (lambda (x) (newline) (display x)) (list 57 321 88))
