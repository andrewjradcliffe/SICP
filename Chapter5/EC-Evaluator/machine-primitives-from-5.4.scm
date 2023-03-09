;; Machine primitives from Section 5.4 necessary for EC-Eval machine

(load "~/aradclif/scheme-projects/SICP/Chapter5/EC-Evaluator/machine-primitives-from-4.1.scm")

(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))
(define (no-more-exps? seq) (null? seq))

(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)
