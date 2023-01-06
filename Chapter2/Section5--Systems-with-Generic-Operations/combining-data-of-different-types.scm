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

