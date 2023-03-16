;; Compiler with addition of open-coded primitives
#|
Strictly for test of Ex. 5.38
|#

;; Under normal circumstances, one would load just the compiler, but we also need a
;; machine simulator. As the machine primitives differ from the compiled-code-evaluator
;; without open-coded primitives, we just re-define things here.
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-register-machine-with-stack-monitoring.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/vanilla-compiler.scm")

;; Ex. 5.38

;; all-regs should be re-defined to include arg1, arg2
(define all-regs '(env proc val argl continue arg1 arg2))

;; a
;; In fact, this can be made more succinct by eliminating the unnecessary
;; assignments to val, followed by assignment to arg1 or arg2.
;; This eliminates 2 useless assign instructions per open-coded primitive,
;; a significant savings! (given that everything reduces to primitives).
(define (spread-arguments operands-list)
  (let ((op-code-2 (compile (cadr operands-list) 'arg2 'next)))
    (preserving '(env)
                op-code-2
                ;; An idiosyncratic but systematic way to express:
                ;; preserve arg2 only if the compilation of the first argument modifies it
                (preserving '(arg2)
                            (compile (car operands-list) 'arg1 'next)
                            (make-instruction-sequence '(arg2) '()
                                                       '())))))


;; b
(define (compile-open-code exp target linkage)
  (let ((argument-code (spread-arguments (operands exp)))
        (op (operator exp)))
    (preserving '(env continue)
                argument-code
                (end-with-linkage linkage
                                  (make-instruction-sequence '(arg1 arg2) (list target)
                                                             `((assign ,target
                                                                       (op ,op)
                                                                       (reg arg1)
                                                                       (reg arg2))))))))

(define (application-open-code? exp)
  (if (pair? exp)
      (let ((op (car exp)))
        (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '=)))
      false))


;; modified compile
(define (compile exp target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage))
        ((assignment? exp)
         (compile-assigment exp target linkage))
        ((definition? exp)
         (compile-definition exp target linkage))
        ((if? exp)
         (compile-if exp target linkage))
        ((lambda? exp)
         (compile-lambda exp target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage))
        ((application-open-code? exp)
         (compile-open-code exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;; in addition to the standard machine primitives, we now need to expose the
;; primitives we plan to open-code
(define compiled-code-operations
  (list
   ;; machine primitives from Scheme
   (list 'cons cons)
   (list 'list list)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '= =)
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
