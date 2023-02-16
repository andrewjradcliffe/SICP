;; The vanilla evaluator, with added syntax from 4.1.2, better handling of
;; internal definitions (4.1.6) and letrec.
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
        ((let*? exp)
         (eval (let*->nested-lets exp) env))
        ((letrec? exp)
         (eval (letrec->let exp) env))
        ;; ;; Iteration constructs
        ;; ((while? exp)
        ;;  (eval (while->nested-let exp) env))
        ;; ((until? exp)
        ;;  (eval (until->named-let exp) env))
        ;; ((do? exp)
        ;;  (eval (do->named-let exp) env))
        ;; ((for? exp)
        ;;  (eval (for->named-let exp) env))
        ;; ((repeat? exp)
        ;;  (eval (repeat->for exp) env))
        ;; Better handling of not than using implementation language primitive
        ((not? exp)
         (eval (not->if exp) env))
        ;;;;;;;;;;;;;;;; end of extra syntax
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

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

;; ;; test of named-let
;; (define (fib n)
;;   (let fib-iter ((a 1)
;;                  (b 0)
;;                  (count n))
;;     (if (= count 0)
;;         b
;;         (fib-iter (+ a b) a (- count 1)))))


;; ;; Ex. 4.9

;; ;; Without a hygienic macro system, these may have name conflicts! Beware!

;; (define (while? exp) (tagged-list? exp 'while))
;; (define (while-predicate exp) (cadr exp))
;; (define (while-body exp) (caddr exp))
;; (define (make-while predicate-exp body-exp)
;;   (list 'while predicate-exp body-exp))


;; (define (while->nested-let exp)
;;   (make-named-let 'while-iter '() (list (make-if (list 'not (while-predicate exp))
;;                                                  'false
;;                                                  (make-begin (list
;;                                                               (while-body exp)
;;                                                               (list 'while-iter)))))))

;; (define (until? exp) (tagged-list? exp 'while))
;; (define (until-predicate exp) (cadr exp))
;; (define (until-body exp) (caddr exp))
;; (define (make-until predicate-exp body-exp)
;;   (list 'until predicate-exp body-exp))

;; (define (until->named-let exp)
;;   (make-named-let 'until-iter '() (list (make-begin (list (until-body exp)
;;                                                           (make-if (until-predicate exp)
;;                                                                    'true
;;                                                                    (list 'until-iter)))))))


;; ;; The infinite loop version
;; (define (do? exp) (tagged-list? exp 'do))
;; (define (do-body exp) (cadr exp))
;; (define (make-do body-exp)
;;   (list 'do body-exp))

;; (define (do->while exp)
;;   (make-while 'true (do-body exp)))

;; (define (do->named-let exp)
;;   (make-named-let 'do-iter '() (list (make-begin (list (do-body exp)
;;                                                        (list 'do-iter))))))


;; (define (for? exp) (tagged-list? exp 'for))
;; (define (for-lb exp) (cadr exp))
;; (define (for-ub exp) (caddr exp))
;; (define (for-lb-var exp) (car (for-lb exp)))
;; (define (for-ub-var exp) (car (for-ub exp)))
;; (define (for-next exp) (cadddr exp))
;; (define (for-cmp exp) (caddddr exp))
;; (define (for-body exp) (cadddddr exp))

;; (define (caddddr x) (car (cddddr x)))
;; (define (cdddddr x) (cdr (cddddr x)))
;; (define (cadddddr x) (car (cdddddr x)))

;; (define (make-for lb ub next cmp body)
;;   (list 'for lb ub next cmp body))

;; (define (for->named-let exp)
;;   (make-named-let 'for-iter (list (for-lb exp) (for-ub exp))
;;                   (list (make-if (list 'not (for-cmp exp))
;;                                  'false
;;                                  (make-begin
;;                                   (list (for-body exp)
;;                                         (list 'for-iter (for-next exp) (for-ub-var exp))))))))


;; (define (repeat? exp) (tagged-list? exp 'repeat))
;; (define (repeat-n exp) (cadr exp))
;; (define (repeat-body exp) (caddr exp))
;; (define (make-repeat n-exp body-exp)
;;   (list 'repeat n-exp body-exp))

;; (define (repeat->for exp)
;;   (make-for (cons 'i-iter 0)
;;             (cons 'm-iter (repeat-n exp))
;;             (list + 'i-iter 1)
;;             (list < 'i-iter 'm-iter)
;;             (repeat-body exp)))

;; Better handling of not

(define (not? exp) (tagged-list? exp 'not))
(define (not-arg exp) (cadr exp))
(define (not->if exp)
  (make-if (not-arg exp) 'false 'true))


;;;;;;;;;;;;;;;; Additions from 4.1.6

;; Ex. 4.16

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

;;;;;;;;;;;;;;;; Tests
;; (define the-global-environment (setup-environment))
;; (driver-loop)

;; (define (f x)
;;   (letrec ((even?
;;             (lambda (n)
;;               (if (= n 0)
;;                   true
;;                   (odd? (- n 1)))))
;;            (odd?
;;             (lambda (n)
;;               (if (= n 0)
;;                   false
;;                   (even? (- n 1))))))
;;     (even? (* x 2))))

;; (f 10)


;; ((lambda (n)
;;    (define a 1)
;;    (define b 2)
;;    (+ a b n))
;;  10)
;; (define (g n)
;;   (define a 1)
;;   (define b 2)
;;   (+ a b n))
;; (g 10)

;; ;; Fails as expected.
;; (let ((a 1))
;;   (define (f x)
;;     (define b (+ a x))
;;     (define a 5)
;;     (+ a b))
;;   (f 10))
