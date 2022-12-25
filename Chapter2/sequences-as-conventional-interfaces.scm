;; 2.2.3 Sequences as Conventional Interfaces

;;;; Sequence Operations
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Ex. 2.33

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Variant using fold-left
;; (define (my-length-left sequence)
;;   (fold-left (lambda (x y) (+ x 1)) 0 sequence))

;; Ex. 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(= (horner-eval 2 (list 1 2 3)) 17)

;; Ex. 2.35
(define (count-leaves t)
  (accumulate + 0 (my-map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1))
                       t)))

(= (count-leaves (list (list 1 2) (list 3 (list 1)))) 4)

;; Ex. 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (my-map car seqs))
            (accumulate-n op init (my-map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(equal? (accumulate-n + 0 s) (list 22 26 30))

;; Ex. 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product r v)) m))

(define (transpose m)
  (accumulate-n cons () m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))


;; Ex. 2.38
(= (fold-right / 1 (list 1 2 3)) (/ 3 2))
(= (fold-left / 1 (list 1 2 3)) (/ 1 6))
(equal? (fold-right list () (list 1 2 3)) (list 1 (list 2 (list 3 ()))))
(equal? (fold-left list () (list 1 2 3)) (list (list (list () 1) 2) 3))

;; Ex. 2.39
(define (reverse-right sequence)
  (fold-right (lambda (x y) (append y (list x))) () sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y) (cons y x)) () sequence))

(define (copy-right sequence)
  (fold-right cons () sequence))

(define (copy-left sequence)
  (fold-left (lambda (x y) (append x (list y))) () sequence))

(equal? (reverse-right (list 1 2 3)) (list 3 2 1))
(equal? (reverse-left (list 1 2 3)) (list 3 2 1))
(equal? (copy-right (list 1 2 3)) (list 1 2 3))
(equal? (copy-left (list 1 2 3)) (list 1 2 3))

(define (deep-reverse-right sequence)
  (fold-right (lambda (x y) (append y (if (pair? x)
                                          (list (deep-reverse-right x))
                                          (list x))))
              ()
              sequence))

(define (deep-reverse-left sequence)
  (fold-left (lambda (x y) (cons (if (pair? y) (deep-reverse-left y) y) x)) () sequence))

(deep-reverse-right '(1 2 3 (1 2)))
(deep-reverse-left '(1 2 3 (1 2)))

(define (last-left sequence)
  (fold-left (lambda (x y) y) () sequence))

(define (last-right sequence) ;; contrived, and also fails on the empty list
  (car (fold-right (lambda (x y) (if (null? y) (cons x y) y)) () sequence)))

(define (last-pair-left sequence)
  (cons (last-left sequence) ()))


;;;; Nested Mappings

(define (flatmap proc seq)
  (accumulate append () (map proc seq)))


;;;; Common utilities
(define (list-ref n items)
  (if (= n 0)
      (car items)
      (list-ref (- n 1) (cdr items))))

(define (enumerate-interval low hi)
  (if (> low hi)
      ()
      (cons low (enumerate-interval (+ low 1) hi))))
