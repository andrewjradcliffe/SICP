;; The amb evaluator, based on the analyzing evaluator which includes
;; the syntax from 4.1.2, better handling of internal definitions (4.1.6) and letrec.

(load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/the-vanilla-with-extra-almond-evaluator.scm")

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ;;;;;;;;;;;;;;;; amb-related
        ((amb? exp) (analyze-amb exp))
        ;;;;;;;;;;;;;;;; start of extra syntax
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((let*? exp) (analyze (let*->nested-lets exp)))
        ((letrec? exp) (analyze (letrec->let exp)))
        ;; Better handling of not than using implementation language primitive
        ((not? exp) (analyze (not->if exp)))
        ;;;;;;;;;;;;;;;; end of extra syntax
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

;; Before we analyze the sequence, we must scan out the defines
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;; Must re-define to overwrite definition used by non-analyzing evaluator.
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
