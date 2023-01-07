;; 2.5.3. Example: Symbolic Algebra

;; Ex. 2.87

;; within install-polynomial-package

;; Exhaustive, and also does not use abstractions of first-term, rest-term
(define (=all-zero-terms? x)
  (if (empty-termlist? x)
      true
      (accumulate (lambda (a b) (and b (=zero? a))) true (map coeff x))))
(define (=zero-poly? p) (=all-zero-terms? (term-list p)))
;; Stops as soon as possible
(define (=zero-poly? x)
  (cond ((empty-termlist? x) true)
        ((not (=zero? (coeff (first-term x)))) false)
        (else (=zero-poly? (rest-terms x)))))

(put '=zero? '(polynomial) (lambda (p) (=zero-poly? (term-list p))))

;; Ex. 2.88

;; generic negation
(define (negate x) (apply-generic 'negate x))
;; within install-scheme-number-package
(put 'negate '(scheme-number) (lambda (x) (tag (* -1 x))))
;; within install-rational-package
(define (negate-rat x) (make-rat (* -1 (numer x)) (denom x)))
(put 'negate '(rational) (lambda (x) (tag (negate-rat x))))
;; within install-complex-package
(define (negate-complex z)
  (make-from-real-imag (negate (real-part z)) (negate (imag-part z))))
(put 'negate '(complex) (lambda (z) (tag (negate-complex z))))

;; within install-polynomial-package

(define (sub-poly p1 p2) (add-poly p1 (negate-poly p2)))
(put 'sub '(polynomial polynomial) (lambda (p1 p2) (tag (sub-poly p1 p2))))
(define (negate-term t) (make-term (order t) (negate (coeff t))))
(define (negate-terms L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (adjoin-term (negate-term (first-term L))
                   (negate-terms (rest-terms L)))))
(define (negate-poly p) (make-poly (variable p) (negate-terms (term-list p))))
(put 'negate '(polynomial) (lambda (p) (tag (negate-poly p))))

;; Ex. 2.89

(define (first-term term-list) (list (- (length term-list) 1) (car term-list)))
(define (adjoin-term term term-list)
  (let ((highest-order (- (length term-list) 1)))
    (let ((diff (- highest-order (order term))))
      (cond ((= diff -1) ;; covers empty-list case as well as case of just 1 higher
             (cons (coeff term) term-list))
            ((< diff -1) ;; |diff| - 1 is number of zeros to add
             (append (cons (coeff term) (zeros (- (abs diff) 1))) term-list))
            ((> diff -1)
             (error "Cannot adjoin term of equal or lesser order" (list term termlist)))))))

(define (zeros n)
  (define (iter lo hi)
    (if (> lo hi)
        '()
        (cons 0 (iter (+ lo 1) hi))))
  (iter 1 n))

;; More general, but probably not as efficient
(define (repeat x n)
  (define (iter lo hi)
    (if (> lo hi)
        '()
        (cons x (iter (+ lo 1) hi))))
  (iter 1 n))
(define (zeros n) (repeat 0 n))

;; Ex. 2.90
;; In analogy to rectangular and polar, one creates sparse and dense packages,
;; each supporting the same set of operations. Then, one defines an abstraction which
;; provides a common interface for these operations. A method will be required for
;; coercion of sparse->dense, which will need to dispatch '(sparse dense) on operations
;; such as add and mul. It is simplest to just promote to dense any sparse polynomial
;; when add, mul, etc. occur on mixed args; algebraic simplifications of many sorts
;; are possible, e.g. sparse + dense yields a sparse, but this quickly becomes an
;; intricate design (outside scope of exercise).

;; Ex. 2.91
(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result (div-terms (sub-terms L1 (mul-term-by-all-terms (make-term new-o new-c) L2)) L2)))
                (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                      (cadr rest-of-result))))))))

;; Ex. 2.93
(define (make-rat n d) (cons n d))
;; change all occurrences of +, -, * to add, sub, mul

;; Ex. 2.94

(define (greatest-common-divisor a b) (apply-generic 'greatest-common-divisor a b))

;; within install-polynomial-package
(define (remainder-terms a b) (cadr (div-terms a b)))
(define (gcd-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1) (gcd-terms (term-list p1) (term-list p2)))
      (error "Polys not in same var -- GCD-POLY" (list p1 p2))))
(put 'greatest-common-divisor '(polynomial polynomial) (lambda (a b) (tag (gcd-poly a b))))

;; inside install-scheme-number-package
(put 'greatest-common-divisor '(scheme-number scheme-number) (lambda (a b) (tag (gcd a b))))

;; inside install-rational-package
(define (make-rat n d)
  (let ((g (greatest-common-divisor n d)))
    (cons (div n g) (div d g))))

;; Ex. 2.95
;; P₁ : x² - 2x + 1
;; P₂ : 11x² + 7
;; P₃ : 13x + 5
;; Q₁ : P₁P₂ = 11x⁴ - 22x³ + 18x² - 14x + 7
;; Q₂ : P₁P₃ = 13x³ - 21x² + 3x + 5

;; On the very first division, 11x⁴ / 13x³ = (11/13)x, an infinite term is introduced.
;; Thus, we have inexactness in any non-rational representation.

;; Ex. 2.96

;; a
(define (pseudoremainder-terms L1 L2)
  (let ((o1 (order (first-term L1)))
        (o2 (order (first-term L2)))
        (c (coeff (first-term L2))))
    (let ((factor (expt c (+ 1 (- o1 o2)))))
      (cadr (div-terms (mul-term-by-all-terms (make-term 0 factor) L1) L2)))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (pseudoremainder-terms a b))))

;; b
(define (integer-gcd terms)
  (let ((coeffs (filter (lambda (x) (not (pair? x))) (map coeff terms))))
    (if (null? coeffs)
        1
        (accumulate gcd 0 coeffs))))

(define (remove-common-factor-terms L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (mul-term-by-all-terms (make-term 0 (/ 1 (integer-gcd L))) L)))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      (remove-common-factor-terms a)
      (gcd-terms b (pseudoremainder-terms a b))))

;; Ex 2.97
