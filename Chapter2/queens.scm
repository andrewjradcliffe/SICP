;; Ex. 2.42; Ex. 2.43

(define empty-board ())

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

(define (same-row? x y)
  (= (car x) (car y)))

(define (same-col? x y)
  (= (cadr x) (cadr y)))

(define (same-diagonal? x y)
  (let ((di (- (car x) (car y)))
        (dj (- (cadr x) (cadr y))))
    (= (abs di) (abs dj))))

(same-row? '(1 2) '(1 2))
(same-col? '(1 2) '(2 2))
(same-diagonal? '(1 2) '(2 3))
