;; 2.2.3 Sequences as Conventional Interfaces

;;;; Sequence Operations
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; Ex 2.33

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) () sequence))

(define (my-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (my-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; Variant using fold-left
;; (define (my-length-left sequence)
;;   (fold-left (lambda (x y) (+ x 1)) 0 sequence))

;; Ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(= (horner-eval 2 (list 1 2 3)) 17)

;; Ex 2.35
(define (count-leaves t)
  (accumulate + 0 (my-map (lambda (x)
                         (if (pair? x)
                             (count-leaves x)
                             1))
                       t)))

(= (count-leaves (list (list 1 2) (list 3 (list 1)))) 4)

;; Ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      ()
      (cons (accumulate op init (my-map car seqs))
            (accumulate-n op init (my-map cdr seqs)))))

(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(equal? (accumulate-n + 0 s) (list 22 26 30))
