;; 2.5.2 Combining Data of Different Types

;; The original apply-generic, from p. 196
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Ex 2.81

;; a
;; Infinite loop, as the apply-generic call on the coerced args is identical to
;; the original, leading to the same branch, etc.

;; b
;; Works correctly as-is, but does try coercion when it is clear that no method exists
;; one could exit earlier.

;; c
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (if (eq? type1 type2)
                        (error "No method for these types"
                               (list op type-tags))
                        (let ((a1 (car args))
                              (a2 (cadr args))
                              (t1->t2 (get-coercion type1 type2))
                              (t2->t1 (get-coercion type2 type1)))
                          (cond (t1->t2
                                 (apply-generic op (t1->t2 a1) a2))
                                (t2->t1
                                 (apply-generic op a1 (t2->t1 a2)))
                                (else
                                 (error "No method for these types"
                                        (list op type-tags))))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Ex 2.82

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (and (> (length args) 1) (not (all-eq? (car type-tags) type-tags)))
              (begin
                (define (try-apply-generic ts)
                  (if (null? ts)
                      (error "No method for these types"
                             (list op type-tags))
                      (let ((T->t (get-coercions (car ts) type-tags)))
                        (if (all T->t)
                            (apply apply-generic (cons op (map-apply T->t args)))
                            (try-apply-generic (cdr ts))))))
                (try-apply-generic type-tags))
              (error "No method for these types"
                     (list op type-tags)))))))

;; Errors on first all-same-type operation for which a procedure does not exist.
;; This works for a tower, but would not be sufficient for general hierarchies.
;; Also, it fails to try all the combinations of types, thus, skipping many
;; possible methods -- in fact, it only ties to find a feasible all-same coercion
;; and then looks for that method.
;; A more general ratcheting strategy could be developed to coerce what can be coerced,
;; then trying again.

;; functions used above
(define (all-eq? x seq)
  (accumulate (lambda (a b) (and b (eq? a x))) true seq))

(define (all x)
  (accumulate (lambda (a b) (and b (not (null? a)))) true x))

(define (map-apply fs args)
  (map (lambda (f x) (apply f (list x))) fs args))

(define (identity x) x)

(define (get-coercions t types)
  (map (lambda (x)
         (if (eq? x t)
             identity
             (get-coercion x t)))
       types))

;; a simple test
(define (add-1 x) (+ x 1))
(define (add-2 x) (+ x 2))
(define fs (list add-1 add-2))
(equal? (map-apply fs '(1 1)) '(2 3))
(equal? fs `(,add-1 ,add-2))


;; Ex. 2.83

(define (raise n) (apply-generic 'raise n))

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (/ x y))))
  (put 'make 'integer (lambda (x) (tag x)))
  (define (raise-integer x) (make-rational x 1))
  (put 'raise '(integer) raise-integer)
  (put 'equ? '(integer integer) (lambda (x y) (= (cdr x) (cdr y))))
  'done)
(define (make-integer n) ((get 'make 'integer) n))

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'make 'real (lambda (x) (tag x)))
  (define (raise-real x) (make-complex-from-real-imag x 0))
  (put 'raise '(real) raise-real)
  (put 'equ? '(real real) (lambda (x y) (= (cdr x) (cdr y))))
  'done)
(define (make-real n) ((get 'make 'real) n))
(define (make-real-from-rational x) (raise x))

;; within install-rational-package
(define (rational->real x) (/ (* 1.0 (numer x)) (denom x)))
(define (raise-rational x) (make-real (rational->real x)))
(put 'raise '(rational) raise-rational)

;; within install-complex package
(put 'raise '(complex) identity)

;; Ex 2.84

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((new-args (promote (car args) (cadr args))))
                (if new-args
                    (apply-generic op (car new-args) (cadr new-args))
                    (error "No method for these types"
                           (list op type-tags))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (try-promote t1 t2) (get-tower 'promote-rule (list t1 t2)))
(define (promote-rule t1 t2)
  (if (eq? t1 t2)
      t1
      (let ((new-t (try-promote t1 t2)))
        (if new-t
            new-t
            (try-promote t2 t1)))))

(define (promote x y)
  (let ((t1 (type-tag x))
        (t2 (type-tag y)))
    (let ((new-t (promote-rule t1 t2)))
      (cond ((and (eq? t1 new-t) (eq? t2 new-t))
             (list x y))
            ((eq? t1 new-t)
             (promote x (raise y)))
            ((eq? t2 new-t)
             (promote (raise x) y))
            (else
             (error "Not possible to promote types" (list t1 t2))
             ;; Or, we could signal with '() so as to maintain new-args requirement
             )))))

;; put-tower, get-tower assumed to exist
(define (install-tower)
  (put-tower 'promote-rule '(integer rational) 'rational)
  (put-tower 'promote-rule '(rational real) 'real)
  (put-tower 'promote-rule '(real complex) 'complex)
  (put-tower 'promote-rule '(integer real) 'real)
  (put-tower 'promote-rule '(integer complex) 'complex)
  (put-tower 'promote-rule '(rational complex) 'complex)
  (put-tower 'supertype 'integer 'rational)
  (put-tower 'supertype 'rational 'real)
  (put-tower 'supertype 'real 'complex)
  (put-tower 'subtype 'rational 'integer)
  (put-tower 'subtype 'real 'rational)
  (put-tower 'subtype 'complex 'real)
  'done)

(define (supertype t) (get-tower 'supertype t))
(define (subtype t) (get-tower 'subtype t))
(define (subtype? t1 t2) (eq? t1 (subtype t2)))
(define (supertype? t2 t1) (subtype? t1 t2)) ;; (eq? t2 (supertype t1))
(define (subtype-rec? t1 t2)
  (let ((t (subtype t2)))
    (cond ((null? t) false) ;; assume that false table lookup returns '()
          ((eq? t1 t) true)
          (else (subtype-rec? t1 t)))))
(define (supertype-rec? t1 t2)
  (let ((t (supertype t2)))
    (cond ((null? t) false)
          ((eq? t1 t) true)
          (else (supertype-rec? t1 t)))))

(define (<: t1 t2) (or (eq? t1 t2) (subtype-rec? t1 t2)))
(define (>: t1 t2) (or (eq? t1 t2) (supertype-rec? t1 t2)))

(define (number-type? x)
  (if (pair? x)
      (<: (type-tag x) 'complex)
      false))
