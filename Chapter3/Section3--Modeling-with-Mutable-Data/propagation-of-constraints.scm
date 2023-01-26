;; 3.3.5 Propagation of Constraints

;; Ex. 3.33

(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier v c u)
    (constant 2 v)
    'ok))


;; Ex. 3.34
;; If a does not have a value, then neither m1 nor m2 has a value, thus, all
;; branches would be skipped.
;; If a has a value, then the product is either set to 0 or a * a.
;; The branches which invoke product and either m1 or m2 are never activated;
;; if they were activated, they would be incorrect anyway.
;; Consider a = 7, b = 25 => a = 25/7 -- (25/7)^2 is certainly not 25.
;; If we allow for negative values of b, the results would be further erroneous still!

;; Ex. 3.35

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square has less than 0 -- SQUARER" (get-value b))
            (set-value! a
                        (sqrt (get-value b))
                        me))
        (if (has-value? a)
            (set-value! b
                        (square (get-value a))
                        me))))
  (define (process-forget-value)
    (forget-value! b me)
    (forget-value! a me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)


;; Ex. 3.36
;; See p. 70, 71 for illustration.

;; Ex. 3.37

(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (divisioner x y z)
  (multiplier y z x))

(define (c/ x y)
  (let ((z (make-connector)))
    (divisioner x y z)
    z))

(define (subtracter x y z)
  (adder y z x))

(define (c- x y)
  (let ((z (make-connector)))
    (subtracter x y z)
    z))

