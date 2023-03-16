;; Compiler with lexical addressing
#|
Strictly for test of lexical addressing -- excludes additions from Ex. 5.38
|#

;; Under normal circumstances, one would load just the compiler, but we also need a
;; machine simulator. As the machine primitives differ from the compiled-code-evaluator
;; without open-coded primitives, we just re-define things here.
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-register-machine-with-stack-monitoring.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/vanilla-compiler.scm")


;; modified compile
(define (compile exp target linkage compile-time-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage compile-time-env))
        ((assignment? exp)
         (compile-assigment exp target linkage compile-time-env))
        ((definition? exp)
         (compile-definition exp target linkage compile-time-env))
        ((if? exp)
         (compile-if exp target linkage compile-time-env))
        ((lambda? exp)
         (compile-lambda exp target linkage compile-time-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           compile-time-env))
        ((application? exp)
         (compile-application exp target linkage compile-time-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (compile-definition exp target linkage compile-time-env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next compile-time-env)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence '(env val) (list target)
                                            `((perform (op define-variable!)
                                                       (const ,var)
                                                       (reg val)
                                                       (reg env))
                                              (assign ,target (const ok))))))))

(define (compile-if exp target linkage compile-time-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next compile-time-env))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage compile-time-env))
            (a-code
             (compile
              (if-alternative exp) target linkage compile-time-env)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

(define (compile-sequence seq target linkage compile-time-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-time-env)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next compile-time-env)
                  (compile-sequence (rest-exps seq) target linkage compile-time-env))))

(define (compile-lambda exp target linkage compile-time-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence '(env) (list target)
                                    `((assign ,target
                                              (op make-compiled-procedure)
                                              (label ,proc-entry)
                                              (reg env)))))
        (compile-lambda-body exp proc-entry compile-time-env))
       after-lambda))))
(define (compile-application exp target linkage compile-time-env)
  (let ((proc-code (compile (operator exp) 'proc 'next compile-time-env))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next compile-time-env))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))


;; Ex. 5.40
(define (compile-lambda-body exp proc-entry compile-time-env)
  (let ((formals (lambda-parameters exp)))
    (let ((extended-env
           (extend-compile-time-environment formals compile-time-env)))
      (append-instruction-sequences
       (make-instruction-sequence '(env proc argl) '(env)
                                  `(,proc-entry
                                    (assign env (op compiled-procedure-env) (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg argl)
                                            (reg env))))
       (compile-sequence (lambda-body exp) 'val 'return extended-env)))))

