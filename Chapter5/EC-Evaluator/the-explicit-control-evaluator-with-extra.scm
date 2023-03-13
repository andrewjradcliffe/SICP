;; The Explicit-Control evaluator, with all the extras from 4.1.2-4.1.6

(load "~/aradclif/scheme-projects/SICP/Chapter5/EC-Evaluator/the-explicit-control-evaluator-with-stack-monitoring.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/EC-Evaluator/machine-primitives-from-4.1-extra.scm")

(define eceval-operations
  (list
   ;; machine primitives from Scheme
   (list 'read read)
   ;; machine primitives from 4.1
   (list 'self-evaluating? self-evaluating?)
   (list 'variable? variable?)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)
   (list 'true? true?)
   (list 'false? false?)
   (list 'make-procedure make-procedure)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   ;;;; Additional machine primitives from 4.1
   (list 'cond? cond?)
   (list 'let? let?)
   (list 'let*? let*?)
   (list 'not? not?)
   (list 'letrec? letrec?)
   (list 'cond->if cond->if)
   (list 'let->combination let->combination)
   (list 'let*->nested-lets let*->nested-lets)
   (list 'letrec->let letrec->let)
   ;; machine primitives from 5.4
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)
   (list 'get-global-environment get-global-environment)))

(define ev-cond
  '(ev-cond
    (assign exp (op cond->if) (reg exp))
    (goto (label eval-dispatch))))
(define ev-let
  '(ev-let
    (assign exp (op let->combination) (reg exp))
    (goto (label eval-dispatch))))
(define ev-let*
  '(ev-let*
    (assign exp (op let*->nested-lets) (reg exp))
    (goto (label eval-dispatch))))
(define ev-letrec
  '(ev-letrec
    (assign exp (op letrec->let) (reg exp))
    (goto (label eval-dispatch))))

(define eval-dispatch
  '(eval-dispatch
    (test (op self-evaluating?) (reg exp))
    (branch (label ev-self-eval))
    (test (op variable?) (reg exp))
    (branch (label ev-variable))
    (test (op quoted?) (reg exp))
    (branch (label ev-quoted))
    (test (op assignment?) (reg exp))
    (branch (label ev-assignment))
    (test (op definition?) (reg exp))
    (branch (label ev-definition))
    (test (op if?) (reg exp))
    (branch (label ev-if))
    (test (op lambda?) (reg exp))
    (branch (label ev-lambda))
    (test (op begin?) (reg exp))
    (branch (label ev-begin))
    (test (op cond?) (reg exp))
    (branch (label ev-cond))
    (test (op let?) (reg exp))
    (branch (label ev-let))
    (test (op let*?) (reg exp))
    (branch (label ev-let*))
    (test (op letrec?) (reg exp))
    (branch (label ev-letrec))
    (test (op application?) (reg exp))
    (branch (label ev-application))
    (goto (label unknown-procedure-type))))


(define eceval-controller-text
  `(
    ,@read-eval-print-loop
    ,@print-result
    ,@unknown-expression-type
    ,@unknown-procedure-type
    ,@signal-error
    ,@eval-dispatch
    ,@ev-self-eval
    ,@ev-variable
    ,@ev-quoted
    ,@ev-lambda
    ,@ev-application
    ,@ev-appl-did-operator
    ,@ev-appl-operand-loop
    ,@ev-appl-accumulate-arg
    ,@ev-appl-last-arg
    ,@ev-appl-accum-last-arg
    ,@apply-dispatch
    ,@primitive-apply
    ,@compound-apply
    ,@ev-begin
    ,@ev-sequence
    ,@ev-sequence-continue
    ,@ev-sequence-last-exp
    ,@ev-if
    ,@ev-if-decide
    ,@ev-if-alternative
    ,@ev-if-consequent
    ,@ev-assignment
    ,@ev-assignment-1
    ,@ev-definition
    ,@ev-definition-1
    ,@ev-cond
    ,@ev-let
    ,@ev-let*
    ,@ev-letrec
    ))

(define eceval
  (make-machine
   eceval-registers
   eceval-operations
   eceval-controller-text))

