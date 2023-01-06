;; Ex. 2.42

(define empty-board '())

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

(define (display-queen-row k row)
  (for-each (lambda (x) (display (if (= x k) "■\t" "⋅\t"))) row)
  (newline))

(define (display-queens positions)
  (newline)
  (display positions)
  (newline)
  (newline)
  (let ((row (car (transpose positions))))
    ;; ((row (fold-right (lambda (x y) (cons (car x) y)) '() positions))) ;; Another way
    (for-each (lambda (k) (display-queen-row k row)) (enumerate-interval 1 (length row)))))

(define ans (queens 4))

(safe? 1 (car ans))
(safe? 2 (car ans))
(safe? 3 (car ans))

(same-diagonal? '(3 2) '(2 3))
(safe-or-identical? '(3 2) '(2 3))
(safe-or-identical? '(2 3) '(3 2))

(filter (lambda (p) (safe? 3 p)) ans)

(safe? 4 (car ans))


(define ans (queens 6))
(for-each display-queens ans)

;; Ex 2.43
(define (bad-queens board-size)
  (define (bad-queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (bad-queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (bad-queen-cols board-size))

;; 6 does happen to finish, as does 7; 8 likely may finish, albeit, after a fair amount of time.
;; In fact, the estimate of order n^n is quite accurate.
;; Measurement of (bad-queens 7) is approximately 15 seconds
;; Measurement of (bad-queens 8) is approximately 300 seconds
;; The 300 / 15 = 20 is almost exactly the ratio of times one would estimate from 8^8 / 7^7
(bad-queens 7)
(expt 6 6)
(expt 7 7)
(expt 8 8)
(* 1.0 (/ (expt 8 7) (expt 7 6)))
(* 1.0 (/ (expt 8 8) (expt 7 7)))
