;; The "lazier" evaluator of Section 4.2.3

(load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/the-lazy-evaluator.scm")

(define (quoted-list->lazy-list exp)
  (literal-list->lazy-list (text-of-quotation exp)))

(define (literal-list->lazy-list seq)
  (if (null? seq)
      '()
      (list 'cons (car seq)
            (literal-list->lazy-list (cdr seq)))
      ;; Actually, everything should be recursively quoted
      ;; (list 'cons (list 'quote (car seq))
      ;;       (literal-list->lazy-list (cdr seq)))
      ))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp)
         (if (or (symbol? (text-of-quotation exp)) (number? (text-of-quotation exp)))
             (text-of-quotation exp)
             (eval (quoted-list->lazy-list exp) env)))
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
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))


(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      ;; (if (cons-procedure? output)
      ;;     (user-print (lookup-variable-value 'x (procedure-environment output))))
      (user-print output)))
  (driver-loop))


(define the-global-environment (setup-environment))
(driver-loop)
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))
;; (define unspecific 'Unspecified-return-value)
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))


(define x '(1 2 3))

(define z '((1 2) (3 4)))
