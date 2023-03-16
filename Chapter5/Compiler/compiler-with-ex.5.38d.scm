;; Compiler with addition of open-coded primitives
#|
Strictly for test of Ex. 5.38. This includes 5.38d
|#
;; The loaded file contains everything necessary up to this point.
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/compiler-with-ex.5.38.scm")

;; Needed to provide neutral element for 0-arg procedure call.
(define (neutral-element op)
  (cond ((eq? op '+) 0)
        ((eq? op '*) 1)
        ((eq? op '=) true)
        ((eq? op '>) true)
        ((eq? op '<) true)
        ((eq? op '>=) true)
        ((eq? op '<=) true)
        (else (error "unknown op -- NEUTRAL-ELEMENT" op))))



(define (application-open-code-varargs? exp)
  (if (pair? exp)
      (let ((op (car exp)))
        (and (or (eq? op '+) (eq? op '*))
             (not (= (length (operands exp)) 2))))
      false))

;; Version 2 -- this is the least complicated way to enact varargs, and also the most robust.
(define (op-varargs->nested-2-arg exp)
  (let ((op (operator exp))
        (operands-list (operands exp)))
    (define (iter operands-list)
      (if (= (length operands-list) 2)
          (cons op operands-list)
          (list op
                (car operands-list)
                (iter (cdr operands-list)))))
    (iter operands-list)))

(define (compile-open-code-varargs exp target linkage)
  (let ((op (operator exp))
        (operands-list (operands exp)))
    (cond ((= (length operands-list) 0)
           (end-with-linkage
            linkage
            (make-instruction-sequence '() (list target)
                                       `((assign ,target ,(neutral-element op))))))
          ((= (length operands-list) 1)
           (preserving '(env continue)
                       (compile (car operands-list) 'arg1 'next)
                       (end-with-linkage
                        linkage
                        (make-instruction-sequence '(arg1) (list target)
                                                   `((assign ,target (op ,op) (reg arg1)))))))
          ;; length of 2 caught by non-varargs, hence, else is for 3+
          (else
           (compile (op-varargs->nested-2-arg exp) target linkage)))))


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
        ((application-open-code-varargs? exp)
         (compile-open-code-varargs exp target linkage))
        ((application-open-code? exp)
         (compile-open-code exp target linkage))
        ((application? exp)
         (compile-application exp target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))
