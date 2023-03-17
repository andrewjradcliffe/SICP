;; Compiler with lexical addressing
#|
Strictly for test of lexical addressing -- excludes additions from Ex. 5.38
|#
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/compiler-with-lexical-addressing.scm")

;; all-regs should be re-defined to include arg1, arg2
(define all-regs '(env proc val argl continue arg1 arg2))

(define (spread-arguments operands-list compile-time-env)
  (let ((op-code-2 (compile (cadr operands-list) 'arg2 'next compile-time-env)))
    (preserving '(env)
                op-code-2
                (preserving '(arg2)
                            (compile (car operands-list) 'arg1 'next compile-time-env)
                            (make-instruction-sequence '(arg2) '()
                                                       '())))))
(define (compile-open-code exp target linkage compile-time-env)
  (let ((argument-code (spread-arguments (operands exp) compile-time-env))
        (op (operator exp)))
    (preserving '(env continue)
                argument-code
                (end-with-linkage linkage
                                  (make-instruction-sequence '(arg1 arg2) (list target)
                                                             `((assign ,target
                                                                       (op ,op)
                                                                       (reg arg1)
                                                                       (reg arg2))))))))
(define (neutral-element op)
  (cond ((eq? op '+) 0)
        ((eq? op '*) 1)
        ((eq? op '=) true)
        ((eq? op '>) true)
        ((eq? op '<) true)
        ((eq? op '>=) true)
        ((eq? op '<=) true)
        (else (error "unknown op -- NEUTRAL-ELEMENT" op))))

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
(define (compile-open-code-varargs exp target linkage compile-time-env)
  (let ((op (operator exp))
        (operands-list (operands exp)))
    (cond ((= (length operands-list) 0)
           (end-with-linkage
            linkage
            (make-instruction-sequence '() (list target)
                                       `((assign ,target ,(neutral-element op))))))
          ((= (length operands-list) 1)
           (preserving '(env continue)
                       (compile (car operands-list) 'arg1 'next compile-time-env)
                       (end-with-linkage
                        linkage
                        (make-instruction-sequence '(arg1) (list target)
                                                   `((assign ,target (op ,op) (reg arg1)))))))
          (else
           (compile (op-varargs->nested-2-arg exp) target linkage compile-time-env)))))

;; Open-coding as per Ex. 5.38
;; (define (application-open-code? exp)
;;   (if (pair? exp)
;;       (let ((op (car exp)))
;;         (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '=)))
;;       false))
;; (define (application-open-code-varargs? exp)
;;   (if (pair? exp)
;;       (let ((op (car exp)))
;;         (and (or (eq? op '+) (eq? op '*))
;;              (not (= (length (operands exp)) 2))))
;;       false))

;; More nuanced open-coding as per Ex. 5.44
(define (application-open-code? exp compile-time-env)
  (if (pair? exp)
      (let ((op (operator exp)))
        (and (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '=))
             (eq? 'not-found (find-variable op compile-time-env))))
      false))
(define (application-open-code-varargs? exp compile-time-env)
  (if (pair? exp)
      (let ((op (operator exp)))
        (and (or (eq? op '+) (eq? op '*))
             (not (= (length (operands exp)) 2))
             (eq? 'not-found (find-variable op compile-time-env))))
      false))

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
        ;; Open-coding as per Ex. 5.38
        ;; ((application-open-code-varargs? exp)
        ;;  (compile-open-code-varargs exp target linkage compile-time-env))
        ;; ((application-open-code? exp)
        ;;  (compile-open-code exp target linkage compile-time-env))
        ;; More nuanced open-coding as per Ex. 5.44
        ((application-open-code-varargs? exp compile-time-env)
         (compile-open-code-varargs exp target linkage compile-time-env))
        ((application-open-code? exp compile-time-env)
         (compile-open-code exp target linkage compile-time-env))
        ((application? exp)
         (compile-application exp target linkage compile-time-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))
