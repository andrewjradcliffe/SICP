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


;; Ex. 4.6

;; (let ((<var_1> <exp_1>)
;;       .
;;       .
;;       .
;;       (<var_N> <exp_N>))
;;   <body>)
;;
;; Transformed to derived expression:
;;
;; ((lambda (<var_1> ... <var_N>) <body>)
;;  <exp_1>
;;  .
;;  .
;;  .
;;  <exp_N>)


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
        (cons (cdar bindings) ;; (cadar bindings) if each binding is list instead of pair
              (iter (cdr bindings)))))
  (iter (let-bindings exp)))

(define (let->combination exp)
  (cons (make-lambda (let-variables exp) (let-body exp)) (let-exps exp)))

;; bindings must be a list of pairs (or list of lists)
;; body must be a list
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

;; within eval, prior to application?
((let? exp)
 (eval (let->combination exp) env))


;; Ex. 4.7

;; (let* ((<var_1> <exp_1>)
;;        .
;;        .
;;        .
;;        (<var_N> <exp_N>))
;;   <body>)
;;
;; Transformed to derived expression:
;;
;; (let ((<var_1> <exp_1>))
;;   (let ((<var_2> <exp_2>))
;;     .
;;     .
;;     .
;;     (let ((<var_N> <exp_N>))
;;       <body>)))

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

;; bindings must be a list of pairs (or list of lists)
;; body must be a list
(define (make-let* bindings body)
  (cons 'let* (cons bindings body)))

;; Seemingly, it is sufficient to add a clause to eval.
((let*? exp)
 (eval (let*->nested-lets exp) env))


;; Ex. 4.8

;; (let <var> <bindings> <body>)
;;
;; Transformed to derived expression:
;;
;; Version 1:
;;
;; (begin (define <var> (lambda (<bound-parameters>) <body>))
;;        (let <bindings>
;;          (<var> <bound-parameters>)))
;;
;;
;; Version 2:
;;
;; (begin (define <var> (lambda (<bound-parameters>) <body>))
;;        (<var> <bound-values>))
;;
;;
;; On first glance, it may appear that Version 1 is necessary in case
;; there are any weird uses of mutation involving the bound parameters.
;; However, in the procedure call, all values would be bound in a fresh environment,
;; so any use of those symbols would be resolved from that environment, not the environment
;; created by the let. In essence, Version 1 would bind the same values twice --
;; first in the let lambda, then again in the procedure call that occurs in the body of
;; said lambda.
;; The creation of the second environment appears to be superfluous, as any mutating
;; changes would be confined to the environment created by the procedure itself, i.e.
;; the third environment. Hence, we can simplify to Version 2 without loss of generality.
;; Below, both named-let->sequence versions are provided.

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
        (cons (cdar bindings) ;; (cadar bindings) if each binding is list instead of pair
              (iter (cdr bindings)))))
  (iter (named-let-bindings exp)))


(define (make-named-let var bindings body)
  (cons 'let (cons var (cons bindings body))))

(define (make-define var value)
  (list 'define var value))
(define (make-define-procedure var parameters body)
  (cons 'define (cons (cons var parameters) body)))

;; Version 1:
(define (named-let->sequence exp)
  (list (make-define (named-let-var exp)
                     (make-lambda (named-let-variables exp)
                                  (named-let-body exp)))
        (make-let (named-let-bindings exp)
                  (list (cons (named-let-var exp)
                              (named-let-variables exp))))))

;; Version 2:
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

;; See p. 117-120 for an interesting illustration of why the following does not work.
(define (fib n)
  (let ((fib-iter (lambda (a b count)
                    (if (= count 0)
                        b
                        (fib-iter (+ a b) a (- count 1)))))
        (a 1)
        (b 0)
        (count n))
    (fib-iter a b count)))

;; Expression created by Version 1.
(define (fib-1 n)
  (define fib-iter (lambda (a b count)
                     (if (= count 0)
                         b
                         (fib-iter (+ a b) a (- count 1)))))
  (let ((a 1)
        (b 0)
        (count n))
    (fib-iter a b count)))

;; Expression created by Version 2.
(define (fib-2 n)
  (define fib-iter (lambda (a b count)
                     (if (= count 0)
                         b
                         (fib-iter (+ a b) a (- count 1)))))
  (fib-iter 1 0 n))

;; using named-let
(define (fib-3 n)
  (let fib-iter ((a 1)
                 (b 0)
                 (count n))
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1)))))

;; Ex. 4.9

;; iteration construct: while
;;
;; (while <predicate> <body>)
;;
;; Transformed to derived expression:
;;
;; Version 1:
;;
;; (if (not <predicate>)
;;     'false
;;     (begin <body>
;;            (while <predicate> <body>)))
;;
;;
;;
;; Version 2:
;;
;; (let while-iter ()
;;   (if (not <predicate>)
;;       'false
;;       (begin <body>
;;              (while-iter))))
;;
;;
;; Both versions are written in tail-recursive form -- transpose the order of
;; consequent and alternative to get the non-tail-recursive form.
;; As should be apparent from above, Version 1 is far from ideal for performance,
;; as each evaluation produces a full interpretive evaluation. On the other hand,
;; Version 2 produces a procedure which is simply called recursively.
;; The disadvantage of Version 2 is that it leaves open the possibility of
;; name conflicts between 'while-iter and the code within <predicate> and/or <body>.
;; Assuming that there is a hygienic way to avoid more conflicts with 'while-iter,
;; the formulation in terms of named-let is likely to be most performant as it does
;; not repeat the expansion of the while syntax on each iteration.
;;
;; Actually, use of named-let is just a convenience for defining and calling
;; a locally-defined procedure. It is, however, a useful framework for thinking
;; about iteration constructs. Otherwise, we encounter specifics
;; (i.e. substitute all the expressions that make up the make-named-let call)
;; which offer no generalizations.
;; In essence, the syntactic transformation to a named-let is the proper mental
;; framework. It also strengthens the importance of named-let as a construct.


(define (while? exp) (tagged-list? exp 'while))
(define (while-predicate exp) (cadr exp))
(define (while-body exp) (caddr exp))
(define (make-while predicate-exp body-exp)
  (list 'while predicate-exp body-exp))

;; Version 1:
(define (while->if-while exp)
  (make-if (list 'not (while-predicate exp))
           'false
           (make-begin (list
                        (while-body exp)
                        exp))))

;; Version 2:
(define (while->nested-let exp)
  (make-named-let 'while-iter '() (list (make-if (list 'not (while-predicate exp))
                                                 'false
                                                 (make-begin (list
                                                              (while-body exp)
                                                              (list 'while-iter)))))))

;; within eval, prior to application?
((while? exp)
 (eval (while->nested-let exp) env))


;; An example usage
(define sum 0)
(while (<= sum 7) (begin (newline)
                         (display sum)
                         (set! sum (+ sum 1))))

;; What it translates to:
(define while-iter (lambda () (if (not (<= sum 7))
                                  false
                                  (begin (newline)
                                         (display sum)
                                         (set! sum (+ sum 1))
                                         (while-iter)))))
(while-iter)
;; Using named-let
(let while-iter ()
  (if (not (<= sum 7))
      false
      (begin (newline)
             (display sum)
             (set! sum (+ sum 1))
             (while-iter))))



;; iteration construct: until
;;
;; (until <predicate> <body>)
;;
;; Transformed to derived expression:
;;
;; (let until-iter ()
;;   (begin <body>
;;          (if <predicate>
;;              'true
;;              (until-iter))))

(define (until? exp) (tagged-list? exp 'while))
(define (until-predicate exp) (cadr exp))
(define (until-body exp) (caddr exp))
(define (make-until predicate-exp body-exp)
  (list 'until predicate-exp body-exp))

(define (until->named-let exp)
  (make-named-let 'until-iter '() (list (make-begin (list (until-body exp)
                                                          (make-if (until-predicate exp)
                                                                   'true
                                                                   (list 'until-iter)))))))

;; within eval, prior to application?
((until? exp)
 (eval (until->named-let exp) env))

;; An example usage
(define sum 0)
(until (<= sum 7) (begin (newline)
                         (display sum)
                         (set! sum (+ sum 1))))

;; What it translates to:
(define until-iter (lambda () (begin (newline)
                                     (display sum)
                                     (set! sum (+ sum 1))
                                     (if (<= sum 7)
                                         true
                                         (until-iter)))))
(until-iter)
;; Using named-let
(let until-iter ()
  (begin (newline)
         (display sum)
         (set! sum (+ sum 1))
         (if (<= sum 7)
             true
             (until-iter))))


;; iteration construct: do
;; Assuming that `do' is the infinite loop, then it has the form:
;;
;; (do <body>)
;;
;; Transformed to derived expression:
;;
;; (while 'true <body>)
;;
;; It is preferable to create the named-let specifically for the do,
;; rather than use while. This simplifies the code despite the fact that it can be
;; expressed using while.
;;
;; (let do-iter '()
;;   (begin <body>
;;          (do-iter)))


(define (do? exp) (tagged-list? exp 'do))
(define (do-body exp) (cadr exp))
(define (make-do body-exp)
  (list 'do body-exp))

(define (do->while exp)
  (make-while 'true (do-body exp)))

(define (do->named-let exp)
  (make-named-let 'do-iter '() (list (make-begin (list (do-body exp)
                                                       (list 'do-iter))))))

;; An example
(define sum 0)
(do (begin (newline)
           (display sum)
           (set! sum (+ sum 1))))

;; What it translates to:
(define do-iter (lambda () (begin (newline)
                                  (display sum)
                                  (set! sum (+ sum 1))
                                  (do-iter))))
;; Using named-let
(let do-iter ()
  (begin (newline)
         (display sum)
         (set! sum (+ sum 1))
         (do-iter)))



;; iteration construct: for
;;
;; (for (<var_LB> <exp_LB>) (<var_UB> <exp_UB>) <next> <cmp> <body>)
;;
;; Transformed to derived expression:
;;
;; (let for-iter ((<var_LB> <exp_LB>)
;;                (<var_UB> <exp_UB>))
;;   (if (not <cmp>)
;;       'false
;;       (begin <body>
;;              (for-iter <next> <var_UB>))))

(define (for? exp) (tagged-list? exp 'for))
(define (for-lb exp) (cadr exp))
(define (for-ub exp) (caddr exp))
(define (for-lb-var exp) (car (for-lb exp)))
(define (for-ub-var exp) (car (for-ub exp)))
(define (for-next exp) (cadddr exp))
(define (for-cmp exp) (caddddr exp))
(define (for-body exp) (cadddddr exp))

(define (caddddr x) (car (cddddr x)))
(define (cdddddr x) (cdr (cddddr x)))
(define (cadddddr x) (car (cdddddr x)))

(define (make-for lb ub next cmp body)
  (list 'for lb ub next cmp body))

(define (for->named-let exp)
  (make-named-let 'for-iter (list (for-lb exp) (for-ub exp))
                  (list (make-if (list 'not (for-cmp exp))
                                 'false
                                 (make-begin
                                  (list (for-body exp)
                                        (list 'for-iter (for-next exp) (for-ub-var exp))))))))


;; within eval, prior to application?
((for? exp)
 (eval (for->named-let exp) env))


;; An example
(for (i 0) (n 7) (+ i 1) (<= i n) (begin (newline)
                                         (display (* i n))))

(let for-iter ((i 0)
               (n 7))
  (if (not (<= i n))
      false
      (begin (newline)
             (display (* i n))
             (for-iter (+ i 1) n))))


