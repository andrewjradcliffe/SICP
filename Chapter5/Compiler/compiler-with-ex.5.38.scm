;; Compiler with addition of open-coded primitives
#|
Strictly for test of Ex. 5.38
|#

;; Under normal circumstances, one would load just the compiler, but we also need a
;; machine simulator, hence, we simply load the compiled code evaluator
;; (which includes the vanilla compiler).
;; (load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/vanilla-compiler.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/compiled-code-evaluator.scm")


;; Ex. 5.38

;; a
(define (spread-arguments operands-list)
  (let ((op-code-2
         (append-instruction-sequences
          (compile (cadr operands-list) 'val 'next)
          (make-instruction-sequence '(val) '(arg2)
                                     '((assign arg2 (reg val)))))))
    (preserving '(env)
                op-code-2
                (preserving '(arg2)
                            (compile (car operands-list) 'val 'next)
                            (make-instruction-sequence '(val) '(arg1)
                                                       '((assign arg1 (reg val))))))))

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
