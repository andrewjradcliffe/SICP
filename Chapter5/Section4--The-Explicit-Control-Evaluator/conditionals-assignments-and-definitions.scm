;; 5.4.3 Conditionals, Assignments and Definitions

;; Ex. 5.23

;; entry points to be added to eceval-controller-text
(define ev-cond-text
  '(ev-cond
    (assign exp (op cond->if) (reg exp))
    (goto (label eval-dispatch))))
(define ev-let-text
  '(ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))))
(define ev-let*-text
  '(ev-let*
    (assign exp (op let*->nested-lets) (reg exp))
    (goto (label eval-dispatch))))
(define ev-letrec-text
  '(ev-letrec
    (assign exp (op letrec->let) (reg exp))
    (goto (label eval-dispatch))))
;; Then, add to eval-dispatch, prior to application?
(test (op cond?) (reg exp))
(branch (label ev-cond))
(test (op let?) (reg exp))
(branch (label ev-let))
(test (op let*?) (reg exp))
(branch (label ev-let*))
(test (op letrec?) (reg exp))
(branch (label ev-letrec))

;; Ex. 5.24

(define cond-as-basic-special-form
  '(ev-cond
    (assign unev (op cond-clauses) (reg exp))
    (goto (label ev-cond-clauses))
    ev-cond-clauses
    (assign exp (op first-exp) (reg unev))
    (test (op last-exp?) (reg unev))
    (branch (label ev-cond-last-clause))
    (save continue)
    (save unev)
    (save env)
    (save exp)
    (assign continue (label ev-cond-clause-decide))
    (assign exp (op cond-predicate) (reg exp))
    (goto (label eval-dispatch))
    ev-cond-clause-decide
    (restore exp)
    (restore env)
    (restore unev)
    (restore continue)
    (test (op true?) (reg val))
    (branch (label ev-cond-clause-true))
    ev-cond-clause-continue
    (assign unev (op rest-exps) (reg unev))
    (goto (label ev-cond-clauses))
    ev-cond-clause-true
    (assign unev (op cond-action) (reg exp))
    (save continue)
    (goto (label ev-sequence))
    ev-cond-last-clause
    (assign unev (op cond-action) (reg exp))
    (save continue)
    (goto (label ev-sequence))))
