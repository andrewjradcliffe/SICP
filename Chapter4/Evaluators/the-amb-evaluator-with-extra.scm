;; Extension of the amb evaluator to include extensions from Section 4.3.3
#|
Includes the following special forms: ramb (that is, random branching), if-fail,
permanent-set!
|#

(load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/the-amb-evaluator.scm")

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ;;;;;;;;;;;;;;;; amb-related
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((if-fail? exp) (analyze (if-fail->amb exp)))
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


(define (random-pop-and-rest items)
  (let ((n (length items)))
    (let ((i (random n)))
      (define (iter count old-seq new-seq)
        (if (and (< count n) (not (= i count)))
            (iter (+ count 1)
                  (cdr old-seq)
                  ;; if one wants to preserve the original order, then use:
                  (append new-seq (list (car old-seq)))
                  ;; else:
                  ;; (cons (car old-seq) new-seq)
                  )
            (if (= count n)
                new-seq
                (iter (+ count 1)
                      (cdr old-seq)
                      new-seq))))
      (cons (list-ref items i)
            (iter 0 items '())))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((p-r (random-pop-and-rest choices)))
              (let ((pop (car p-r))
                    (rest (cdr p-r)))
                (pop env
                     succeed
                     (lambda ()
                       (try-next rest)))))))
      (try-next cprocs))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))


(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-normal exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))
(define (if-fail->amb exp) (cons 'amb (cdr exp)))
