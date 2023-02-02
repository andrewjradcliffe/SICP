;; The vanilla evaluator, with added syntax from 4.1.2
#|
Vanilla with almond is excellent, and maintains the minimalist philosophy.
|#


(load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/the-vanilla-evaluator.scm")

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ;;;;;;;;;;;;;;;; start of extra syntax
        ((and? exp)
         (eval (and->if exp) env))
        ((or? exp)
         (eval (or->if exp) env))
        ((let? exp)
         (eval (let->combination exp) env))
        ;;;;;;;;;;;;;;;; end of extra syntax
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))


;; Ex. 4.4

(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))
(define (and-exps exp) (cdr exp))
(define (or-exps exp) (cdr exp))

(define (and->if exp) (and->if-iter (and-exps exp)))
(define (and->if-iter exps)
  (if (null? exps)
      'true
      (make-if (car exps) (if (null? (cdr exps))
                              (car exps)
                              (and->if-iter (cdr exps)))
               'false)))

(define (or->if exp) (or->if-iter (or-exps exp)))
(define (or->if-iter exps)
  (if (null? exps)
      'false
      (make-if (car exps) (car exps) (or->if-iter (cdr exps)))))

;; Ex. 4.5

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

;; Ex. 4.6

(define (let? exp) (tagged-list? exp 'let))
(define (let-body exp) (cddr exp))
(define (let-bindings exp) (cadr exp))

(define (let-variables exp)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (caar bindings)
              (iter (cdr bindings)))))
  (iter (let-bindings exp)))

(define (let-exps exp)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (cadar bindings) ;; cdar if pair
              (iter (cdr bindings)))))
  (iter (let-bindings exp)))

(define (let->combination exp)
  (cons (make-lambda (let-variables exp) (let-body exp)) (let-exps exp)))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))


;; Ex. 4.7

(define (let*? exp) (tagged-list? exp 'let*))
(define (let*-body exp) (cddr exp))
(define (let*-bindings exp) (cadr exp))

(define (let*->nested-lets exp)
  (define (iter bindings)
    (if (null? (cdr bindings))
        (make-let (list (car bindings)) (let*-body exp))
        (make-let (list (car bindings)) (list (iter (cdr bindings))))))
  (if (null? (let*-bindings exp))
      (make-let '() (let*-body exp))
      (iter (let*-bindings exp))))

(define (make-let* bindings body)
  (cons 'let* (cons bindings body)))
