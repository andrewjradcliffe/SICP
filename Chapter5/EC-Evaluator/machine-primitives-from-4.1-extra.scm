;; Machine primitives from 4.1.2-4.1.6 -- rich syntax for EC-Eval machine

;; Syntax transformations and associated utilities
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
  (if (null? clauses)
      'false                ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error ("ELSE clause isn't last -- COND->IF"
                        clauses)))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;;;;;;;;;;;;;;; Additions from 4.1.2
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


;; Ex. 4.8

(define (named-let? exp)
  (if (let? exp)
      (variable? (cadr exp))
      false))
(define (named-let-var exp) (cadr exp))
(define (named-let-body exp) (cdddr exp))
(define (named-let-bindings exp) (caddr exp))
(define (named-let-variables exp)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (caar bindings)
              (iter (cdr bindings)))))
  (iter (named-let-bindings exp)))
(define (named-let-exps exp)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (cadar bindings) ;; (cdar bindings) if each binding is pair instead of list
              (iter (cdr bindings)))))
  (iter (named-let-bindings exp)))


(define (make-named-let var bindings body)
  (cons 'let (cons var (cons bindings body))))

(define (make-define var value)
  (list 'define var value))
(define (make-define-procedure var parameters body)
  (cons 'define (cons (cons var parameters) body)))

(define (named-let->sequence exp)
  (list (make-define (named-let-var exp)
                     (make-lambda (named-let-variables exp)
                                  (named-let-body exp)))
        (cons (named-let-var exp)
              (named-let-exps exp))))

;; Thus, one could handle this by simply changing:
(define (let->combination exp)
  (if (named-let? exp) ;; or (variable? (cadr exp))
      (make-begin (named-let->sequence exp))
      (cons (make-lambda (let-variables exp)
                         (let-body exp))
            (let-exps exp))))


(define (not? exp) (tagged-list? exp 'not))
(define (not-arg exp) (cadr exp))
(define (not->if exp)
  (make-if (not-arg exp) 'false 'true))

;; not using the abstractions from Ex. 4.12
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; Version 3: everything together. As it happens, it is actually not quite as messy
;; as anticipated.
(define (scan-out-defines exps)
  (define (iter bindings set!-exps body-exps exps)
    (if (null? exps)
        (if (null? bindings)
            body-exps
            (list (make-let bindings (append set!-exps body-exps))))
        (let ((first (car exps))
              (rest (cdr exps)))
          (if (definition? first)
              (let ((var (definition-variable first))
                    (val (definition-value first)))
                (iter (append bindings (list (list var ''*unassigned*)))
                      (append set!-exps (list (list 'set! var val)))
                      body-exps
                      rest))
              (iter bindings set!-exps (append body-exps (list first)) rest)))))
  (iter '() '() '() exps))

(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))


;; Ex. 4.20
(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))
(define (letrec-variables exp) (map car (letrec-bindings exp)))
(define (letrec-exps exp) (map cadr (letrec-bindings exp)))

;; Syntax transformation to form in text
(define (letrec->let exp)
  (let ((vars (letrec-variables exp)))
    (make-let (map (lambda (var) (list var ''*unassigned*)) vars)
              (append (map (lambda (var exp) (list 'set! var exp)) vars (letrec-exps exp))
                      (letrec-body exp)))))
