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


(define (safe-or-identical? x y)
  (if (equal? x y)
      true
      (and (not (same-row? x y)) (not (same-col? x y)) (not (same-diagonal? x y)))))

(safe-or-identical? '(1 1) '(2 3))

(define (safe? k positions)
  (let ((queen (list-ref (- k 1) positions)))
    (accumulate (lambda (x y) (and (safe-or-identical? queen x) y))
                true
                positions)))

(safe? 2 '((1 1) (3 2) (2 3)))
(safe? 2 '((1 1) (3 2) (3 3)))
(safe? 2 '((1 1) (3 2) (1 3)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
