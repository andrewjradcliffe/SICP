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


;; Ex. 4.4
;; (and <e_1> ... <e_N>)
;; (or <e_1> ... <e_N>)

;; As special forms

;; Note that it is necessary to use true? here as the implementation language is
;; doing the evaluation, but result is a value from the implemented language.
;; Thus, we need to check whether we consider it to be true from the perspective
;; of the implementation language.

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (eval-and-iter (and-exps exp) env))
(define (and-exps exp) (cdr exp))
(define (eval-and-iter exps env)
  (if (null? exps)
      true
      (let ((result (eval (car exps) env)))
        (if (true? result)
            (if (null? (cdr exps))
                result
                (eval-and-iter (cdr exps) env))
            false))))

(define (eval-or exp env)
  (eval-or-iter (or-exps exp) env))
(define (or-exps exp) (cdr exp))
(define (eval-or-iter exps env)
  (if (null? exps)
      false
      (let ((result (eval (car exps) env)))
        (if (true? result)
            result
            (eval-or-iter (cdr exps) env)))))

;; insert into cond of eval, prior to application?
((and? exp)
 (eval-and exp env))
((or? exp)
 (eval-or exp env))



;; As derived expressions

;; Here we do not need to use true? as the derived expression is produced from
;; special forms (namely, if) which handle the use of true? as needed.
;; In essence, here we are simply expanding an expression into simpler (but more verbose)
;; expressions -- special forms -- which utilize procedures such as true? internally.
;; Conversely, in the special form implementation, we must use true? as we are evaluating
;; the expression directly.
;; In other words, implementations of special forms must take care to remember that
;; the values encountered originate in the implemented language, hence, special purpose
;; utilities such as true? must be used to test logical values, perform primitive operations, etc.

(define (and->if exp) (and->if-iter (and-exps exp)))
(define (and->if-iter exps)
  (if (null? exps)
      'true
      (make-if (car exps) (if (null? (cdr exps))
                              (car exps)
                              (and-if-iter (cdr exps)))
               'false)))

(define (or->if exp) (or->if-iter (or-exps exp)))
(define (or->if-iter exps)
  (if (null? exps)
      'false
      (make-if (car exps) (car exps) (or->if-iter (cdr exps)))))

;; insert into cond of eval, prior to application?
((and? exp)
 (eval (and->if exp) env))
((or? exp)
 (eval (or->if exp) env))


;; Ex. 4.5

;; (cond ((<predicate> <action_1> ... <action_N>))
;;       .
;;       .
;;       .
;;       ((<predicate> <action_1> ... <action_N>)))
;;
;; New clause syntax: (<test> => <recipient>)
;; Corresponds to: (if <test> (<recipient> <test>))

(define (cond-=>-clause? clause)
  (cond ((null? (cond-actions clause)) false)
        ((eq? (car (cond-actions clause)) '=>) true)
        (else false)))

(define (cond-recipient clause)
  (cadr (cond-actions clause)))

(define (cond-=>->exp clause)
  (list (cond-recipient clause) (cond-predicate clause)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (if (cond-=>-clause? first)
                (make-if (cond-predicate first)
                         (cond-=>->exp first)
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))
