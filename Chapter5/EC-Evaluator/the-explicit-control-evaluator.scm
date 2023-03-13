;; 5.4 The Explicit-Control Evaluator
#|
Drawing upon a several sources, hence, a separate directory.
|#

(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-vanilla-register-machine.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/EC-Evaluator/machine-primitives-from-5.4.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/EC-Evaluator/explicit-control-evaluator-controller-text-fragments.scm")

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
   ;; machine primitives from 5.4
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)
   (list 'get-global-environment get-global-environment)))

(define eceval-registers
  '(exp env val proc argl continue unev))

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
    ))

(define eceval
  (make-machine
   eceval-registers
   eceval-operations
   eceval-controller-text))

;; ;;;;;;;;;;;;;;;; Tests
;; (start eceval)
;; (define (append x y)
;;   (if (null? x)
;;       y
;;       (cons (car x)
;;             (append (cdr x) y))))
;; (append '(a b c) '(d e f))
