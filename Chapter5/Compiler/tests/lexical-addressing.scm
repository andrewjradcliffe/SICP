#|
Collected tests for compiler with lexical addressing. These should work with and without
open-coding. Either compiler can be loaded and the examples tested.
|#

;; compilers -- choose one
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/compiler-with-lexical-addressing.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/compiler-with-open-coding-and-lexical-addressing.scm")
;;;;;;;;;;;;;;;;

;; Test case #1
(define factorial-rec
  (begin (reset-label-counter)
         (compile
          '(define (factorial n)
             (if (= n 1)
                 1
                 (* (factorial (- n 1)) n)))
          'val
          'next
          empty-compile-time-environment)))
(print-compiled-instruction-sequence factorial-rec)

(define (compiled-factorial-eval-with-monitoring n)
  (define the-global-environment (setup-environment))
  (define (get-global-environment) the-global-environment)
  (define compiled-machine
    (make-machine
     all-regs
     (append (list (list 'get-global-environment get-global-environment))
             compiled-code-operations)
     `(
       (perform (op initialize-stack))
       ,@(statements
          (begin (reset-label-counter)
                 (compile
                  `(begin
                     (define (factorial n)
                       (if (= n 1)
                           1
                           (* (factorial (- n 1)) n)))
                     (factorial ,n))
                  'val
                  'next
                  empty-compile-time-environment)
                 ))
       (perform (op print-stack-statistics))
       )
     ))
  (set-register-contents! compiled-machine 'env (get-global-environment))
  (start compiled-machine)
  (get-register-contents compiled-machine 'val)
  )
(define (interactive-compiled-factorial-eval-with-monitoring)
  (display ";;; enter n")
  (newline)
  (let ((n (read)))
    (let ((result (compiled-factorial-eval-with-monitoring n)))
      (newline)
      (display "factorial of ")
      (display n)
      (display " = ")
      (display result)))
  (newline)
  (interactive-compiled-factorial-eval-with-monitoring))

;; Test case #2
(define (test-f a b c d e)
  (((lambda (x y)
      (lambda (a b c d e)
        ((lambda (y z) (* x y z))
         (* a b x)
         (+ c d x))))
    3
    4)
   a b c d e))
(define test-f-asm
  (begin (reset-label-counter)
         (compile
          '(define (test-f a b c d e)
             (((lambda (x y)
                 (lambda (a b c d e)
                   ((lambda (y z) (* x y z))
                    (* a b x)
                    (+ c d x))))
               3
               4)
              a b c d e))
          'val
          'next
          empty-compile-time-environment)))
(print-compiled-instruction-sequence test-f-asm)


(define (compiled-test-f-eval-with-monitoring a b c d e)
  (define the-global-environment (setup-environment))
  (define (get-global-environment) the-global-environment)
  (define compiled-machine
    (make-machine
     all-regs
     (append (list (list 'get-global-environment get-global-environment))
             compiled-code-operations)
     `(
       (perform (op initialize-stack))
       ,@(statements
          (begin (reset-label-counter)
                 (compile
                  `(begin
                     (define (test-f a b c d e)
                       (((lambda (x y)
                           (lambda (a b c d e)
                             ((lambda (y z) (* x y z))
                              (* a b x)
                              (+ c d x))))
                         3
                         4)
                        a b c d e))
                     (test-f ,a ,b ,c ,d ,e))
                  'val
                  'next
                  empty-compile-time-environment)
                 ))
       (perform (op print-stack-statistics))
       )
     ))
  (set-register-contents! compiled-machine 'env (get-global-environment))
  (start compiled-machine)
  (get-register-contents compiled-machine 'val)
  )
(define (interactive-compiled-test-f-eval-with-monitoring)
  (display ";;; enter (a b c d e)")
  (newline)
  (let ((abcde (read)))
    (let ((a (first abcde))
          (b (second abcde))
          (c (third abcde))
          (d (fourth abcde))
          (e (fifth abcde)))
      (let ((result (compiled-test-f-eval-with-monitoring a b c d e)))
        (newline)
        (display "test-f of ")
        (display abcde)
        (display " = ")
        (display result)
        (display "    check value of test-f = ")
        (display (test-f a b c d e)))))
  (newline)
  (interactive-compiled-test-f-eval-with-monitoring))

;; Test case #3
(define (test-f-2 a b)
  (define u (* 2 3 a))
  (define v (+ 1 2 3 4 5 6 (* a b) (+ a 2)))
  (define (fact-iter product counter)
    (if (> counter v)
        product
        (fact-iter (* counter product) (+ counter 1))))
  (+ (fact-iter 1 1) u))
(define test-f-2-asm
  (begin (reset-label-counter)
         (compile
          '(define (test-f-2 a b)
            (define u (* 2 3 a))
            (define v (+ 1 2 3 4 5 6 (* a b) (+ a 2)))
            (define (fact-iter product counter)
              (if (> counter v)
                  product
                  (fact-iter (* counter product) (+ counter 1))))
            (+ (fact-iter 1 1) u))
          'val
          'next
          empty-compile-time-environment)))
(print-compiled-instruction-sequence test-f-2-asm)


(define (compiled-test-f-2-eval-with-monitoring a b)
  (define the-global-environment (setup-environment))
  (define (get-global-environment) the-global-environment)
  (define compiled-machine
    (make-machine
     all-regs
     (append (list (list 'get-global-environment get-global-environment))
             compiled-code-operations)
     `(
       (perform (op initialize-stack))
       ,@(statements
          (begin (reset-label-counter)
                 (compile
                  `(begin
                     (define (test-f-2 a b)
                       (define u (* 2 3 a))
                       (define v (+ 1 2 3 4 5 6 (* a b) (+ a 2)))
                       (define (fact-iter product counter)
                         (if (> counter v)
                             product
                             (fact-iter (* counter product) (+ counter 1))))
                       (+ (fact-iter 1 1) u))
                     (test-f-2 ,a ,b))
                  'val
                  'next
                  empty-compile-time-environment)
                 ))
       (perform (op print-stack-statistics))
       )
     ))
  (set-register-contents! compiled-machine 'env (get-global-environment))
  (start compiled-machine)
  (get-register-contents compiled-machine 'val)
  )
(define (interactive-compiled-test-f-2-eval-with-monitoring)
  (display ";;; enter (a b)")
  (newline)
  (let ((ab (read)))
    (let ((a (car ab))
          (b (cadr ab)))
      (let ((result (compiled-test-f-2-eval-with-monitoring a b)))
        (newline)
        (display "test-f-2 of ")
        (display ab)
        (display " = ")
        (display result)
        (display "    check value of test-f-2 = ")
        (display (test-f-2 a b)))))
  (newline)
  (interactive-compiled-test-f-2-eval-with-monitoring))

;; Test case #4
(define (test-f-3 a b)
  (define u 1)
  (define v 2)
  (+ u v a b))
(define test-f-3-asm
  (begin (reset-label-counter)
         (compile
          '(define (test-f-3 a b)
             (define u 1)
             (define v 2)
             (+ u v a b))
          'val
          'next
          empty-compile-time-environment)))
(print-compiled-instruction-sequence test-f-3-asm)

(define (compiled-test-f-3-eval-with-monitoring a b)
  (define the-global-environment (setup-environment))
  (define (get-global-environment) the-global-environment)
  (define compiled-machine
    (make-machine
     all-regs
     (append (list (list 'get-global-environment get-global-environment))
             compiled-code-operations)
     `(
       (perform (op initialize-stack))
       ,@(statements
          (begin (reset-label-counter)
                 (compile
                  `(begin
                     (define (test-f-3 a b)
                       (define u 1)
                       (define v 2)
                       (+ u v a b))
                     (test-f-3 ,a ,b))
                  'val
                  'next
                  empty-compile-time-environment)
                 ))
       (perform (op print-stack-statistics))
       )
     ))
  (set-register-contents! compiled-machine 'env (get-global-environment))
  (start compiled-machine)
  (get-register-contents compiled-machine 'val)
  )
(define (interactive-compiled-test-f-3-eval-with-monitoring)
  (display ";;; enter (a b)")
  (newline)
  (let ((ab (read)))
    (let ((a (car ab))
          (b (cadr ab)))
      (let ((result (compiled-test-f-3-eval-with-monitoring a b)))
        (newline)
        (display "test-f-3 of ")
        (display ab)
        (display " = ")
        (display result)
        (display "    check value of test-f-3 = ")
        (display (test-f-3 a b)))))
  (newline)
  (interactive-compiled-test-f-3-eval-with-monitoring))
