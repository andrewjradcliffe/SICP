;; Machine with primitives for execution of compiled code

(load "~/aradclif/scheme-projects/SICP/Chapter5/EC-Evaluator/the-explicit-control-evaluator-with-stack-monitoring.scm")

;; Ensure a clean environment
(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)

(define compiled-code-operations
  (list
   ;; machine primitives from Scheme
   (list 'cons cons)
   (list 'list list)
   ;; truth
   (list 'true? true?)
   (list 'false? false?)
   ;; environment operations
   (list 'lookup-variable-value lookup-variable-value)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
   (list 'extend-environment extend-environment)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   ))


(define compiled-machine
  (make-machine
   all-regs
   compiled-code-operations
   `(
     (perform (op initialize-stack))
     ,@(statements
      (begin (reset-label-counter)
             (compile
              '(begin
                 (define (factorial n)
                   (if (= n 1)
                       1
                       (* (factorial (- n 1)) n)))
                 (factorial 6))
              'val
              'next)
             ))
     (perform (op print-stack-statistics))
     )
   ))
(set-register-contents! compiled-machine 'env (get-global-environment))
(start compiled-machine)
(get-register-contents compiled-machine 'val)
