;; 4.1.2 Representing Expressions

;; Ex. 4.2

;; a

;; In general, this will result in non-reduced combinations be applied to
;; potentially non-reduced operands. Another way to look at this is that all
;; expressions -- assignment, definition, if, lambda, begin, cond -- will be
;; treated in the application? branch as they all satisfy the predicate (pair? exp).
;; Thus, (apply (eval (operator exp) env)
;;              (list-of-values (operands exp) env))
;; is the body called on anything which is not self-evaluating, a variable, or quoted.

;; (define x 3) would result in eval being called on 'define, which would result in
;; lookup-variable-value being called on 'define in the enclosing environment.
;; Perhaps 'define could be looked up and returned as a procedure? (doubtful)
;; The list-of-values call on the operands would call eval on 'x, the corresponding
;; lookup-variable-value for which would assuredly fail, as it would not be possible
;; to define any variables (other than built-ins, which we assume to be magically available)
;; given Louis' proposed change.

;; Therefore, while it may be possible to return a procedure for 'define, assuming that
;; we somehow manage to get the evaluator working, the attempted lookup for 'x would fail.
;; Even if it did not fail, it would not have the intended effect as it is unclear
;; what would be recipient of the definition.

;; b

(define (application? exp)
  (tagged-list? exp 'call))

(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))


;; Ex. 4.3

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
         (if (pair? exp)
             (let ((eval-type (get 'eval (car exp))))
               (if eval-type
                   (eval-type exp env)
                   (apply (eval (operator exp) env)
                          (list-of-values (operands exp) env))))
             (error "Unknown expression type -- EVAL" exp)))))

(define (eval-quoted exp env) (text-of-quotation exp))
(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))
(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))
(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (install-evaluator)
  (put 'eval 'quote eval-quoted)
  (put 'eval 'set! eval-assignment)
  (put 'eval 'define eval-definition)
  (put 'eval 'if eval-if)
  (put 'eval 'lambda eval-lambda)
  (put 'eval 'begin eval-begin)
  (put 'eval 'cond eval-cond)
  'done)


