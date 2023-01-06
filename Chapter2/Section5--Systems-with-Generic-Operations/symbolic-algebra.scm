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

;; 2.90
;; In analogy to rectangular and polar, one creates sparse and dense packages,
;; each supporting the same set of operations. Then, one defines an abstraction which
;; provides a common interface for these operations. A method will be required for
;; coercion of sparse->dense, which will need to dispatch '(sparse dense) on operations
;; such as add and mul. It is simplest to just promote to dense any sparse polynomial
;; when add, mul, etc. occur on mixed args; algebraic simplifications of many sorts
;; are possible, e.g. sparse + dense yields a sparse, but this quickly becomes an
;; intricate design (outside scope of exercise).


