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
 (if (or (symbol? (text-of-quotation exp)) (number? (text-of-quotation exp)))
     (text-of-quotation exp)
     (eval (quoted-list->lazy-list exp) env)))


;; Ex. 4.34

;; A simple approach

(define (cons-procedure? p)
  (if (compound-procedure? p)
      (and (equal? '(m) (procedure-parameters p))
           (equal? '((m x y)) (procedure-body p)))
      false))

;; Attempt 1: Operating on the procedures themselves is error-prone.
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


;; Attempt 2: Another failed attempt. Using the implemented language itself is problematic
;; as printing of procedures is guaranteed to be a problem -- unless one installs
;; a primitive procedure?, but that would only allow one to prevent the printing
;; of procedure objects as literals. How to print these would then become an adventure
;; in exposing the implementation language to the implemented language -- clearly,
;; such circular reasoning is best avoided from the start.
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (if (cons-procedure? output)
          ;; Quite a mistake to forget that cons is lazy!
          ;; (let ((vals (actual-value (list 'map '(lambda (x) x) input) the-global-environment)))
          ;;   (let ((first (car vals))
          ;;         (rest (cdr vals)))
          ;;     (display "(")
          ;;     (user-print first)
          ;;     (for-each (lambda (x) (display " ") (user-print x)) rest)
          ;;     (display ")")))
          ;; Safe only for lists of primitive types!
          (begin (actual-value (list
                                'begin
                                (list 'display "(")
                                (list 'display (list 'car input))
                                (list 'for-each '(lambda (x) (display " ") (display x))
                                      (list 'cdr input))
                                (list 'display ")")
                                )
                               the-global-environment)
                 ;; (user-print (actual-value (list 'car input) the-global-environment))
                 )
          (user-print output))
      ;; (user-print output)
      ))
  (driver-loop))

;; Attempt 3: The solution. In essence, carefully force the components.
;; Sadly, these are defective. They print pairs properly, but lists
;; are printed as nested pairs.
;; e.g. (cons 1 (cons 2 '()))    =>    (1 (2))
(define (user-print object)
  (cond ((cons-procedure? object)
         (display "(")
         (let ((x (force-it (lookup-variable-value 'x (procedure-environment object))))
               (y (force-it (lookup-variable-value 'y (procedure-environment object)))))
           (user-print x)
           ;; This can be slightly fancier, to handle the empty list at the end of a list.
           (if (cons-procedure? y)
               (display " ")
               (display " . "))
           (user-print y))
         (display ")"))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))

;; This can be slightly fancier, to handle the empty list at the end of a list.
(define (user-print object)
  (cond ((cons-procedure? object)
         (display "(")
         (let ((x (force-it (lookup-variable-value 'x (procedure-environment object))))
               (y (force-it (lookup-variable-value 'y (procedure-environment object)))))
           (user-print x)
           (cond ((cons-procedure? y)
                  (display " ")
                  (user-print y))
                 ((null? y)
                  (display ""))
                 (else
                  (display " . ")
                  (user-print y))))
         (display ")"))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))



;; Attempt 4: Printing of lists identical to Scheme.
;; In essence, once we begin printing a list (or pair), print the left parenthesis,
;; print all internal contents, then print the right parenthesis.
(define (user-print object)
  (cond ((cons-procedure? object)
         (display "(")
         (let ((x (force-it (lookup-variable-value 'x (procedure-environment object))))
               (y (force-it (lookup-variable-value 'y (procedure-environment object)))))
           (user-print x)
           (user-print-list y))
         (display ")"))
        ((compound-procedure? object)
         (display (list 'compound-procedure
                        (procedure-parameters object)
                        (procedure-body object)
                        '<procedure-env>)))
        (else (display object))))

(define (user-print-list object)
  (cond ((cons-procedure? object)
         (display " ")
         (let ((x (force-it (lookup-variable-value 'x (procedure-environment object))))
               (y (force-it (lookup-variable-value 'y (procedure-environment object)))))
           (user-print x)
           (user-print-list y)))
        ((null? object)
         (display ""))
        (else
         (display " . ")
         (user-print object))))
