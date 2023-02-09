;; 4.2.3 Streams as Lazy Lists

;; Ex. 4.32
#|
The primary difference is in the delay of the car, which streams lack.
Thus, anything that relies on and/or can exploit a delayed car will
be able to take advantage of the extra laziness.

A useful example can be constructed from the bank account example, where
the balance in the car need not be evaluated explicitly, as it was for the stream.
This could be used to construct an account which is a function of as-to-yet
unresolved variables.
|#

;; Ex. 4.33

(define (quoted-list->lazy-list exp)
  (literal-list->lazy-list (text-of-quotation exp)))

(define (literal-list->lazy-list seq)
  (if (null? seq)
      '()
      ;; (list 'cons (car seq)
      ;;       (literal-list->lazy-list (cdr seq)))
      ;; in terms of cons, for fun
      (cons 'cons (cons (car seq))
            (cons (literal-list->lazy-list (cdr seq))
                  '()))
      ))

;; within eval:
((quoted? exp)
 (if (symbol? (text-of-quotation exp))
     (text-of-quotation exp)
     (quoted-list->lazy-list exp)))


;; Ex. 4.34

;; A simple approach

(define (cons-procedure? p)
  (if (compound-procedure? p)
      (and (equal? '(m) (procedure-parameters p))
           (equal? '((m x y)) (procedure-body p)))
      false))

(define (user-print object)
  (cond ((cons-procedure? object)
         (display "(")
         (display (actual-value (list 'car object) the-global-environment))
         (let ((next (actual-value (list 'cdr object) the-global-environment)))
           (if (cons-procedure? next)
               (display " ")
               (display " . "))
           (user-print next))
         (display ")"))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))

