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


;; Test case #5
(define test-f-4-asm
  (begin (reset-label-counter)
         (compile
          '(begin
             (define a 1)
             (define b 2)
             (define (test-f-4)
               (define u 1)
               (define v 2)
               (+ u v a b)))
          'val
          'next
          empty-compile-time-environment)))
(print-compiled-instruction-sequence test-f-4-asm)


(define (compiled-test-f-4-eval-with-monitoring a b)
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
                     (define a ,a)
                     (define b ,b)
                     (define (test-f-4)
                       (define u 1)
                       (define v 2)
                       (+ u v a b))
                     (test-f-4))
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
(define (interactive-compiled-test-f-4-eval-with-monitoring)
  (display ";;; enter (a b)")
  (newline)
  (let ((ab (read)))
    (let ((a (car ab))
          (b (cadr ab)))
      (let ((result (compiled-test-f-4-eval-with-monitoring a b)))
        (newline)
        (display "test-f-4 of ")
        (display ab)
        (display " = ")
        (display result)
        (display "    check value of test-f-4 = ")
        (display (test-f-3 a b)))))
  (newline)
  (interactive-compiled-test-f-4-eval-with-monitoring))

;; Test case #6
;; Only after Ex. 5.44 will the open-coding compiler pass this test
#|
A: 2x2 in row-major order
A_11 : car
A_12 : cadr
A_21 : caddr
A_22 : cadddr
|#
(define (linear-combination + * a b x y)
  (+ (* a x) (* b y)))
(define (+matrix-2-by-2 A B)
  (list (+ (car A) (car B))
        (+ (cadr A) (cadr B))
        (+ (caddr A) (caddr B))
        (+ (cadddr A) (cadddr B))))
(define (*matrix-2-by-2 A B)
  (list (+ (* (car A) (car B)) (* (cadr A) (caddr B)))
        (+ (* (car A) (cadr B)) (* (cadr A) (cadddr B)))
        (+ (* (caddr A) (car B)) (* (cadddr A) (caddr B)))
        (+ (* (caddr A) (cadr B)) (* (cadddr A) (cadddr B)))))
(define (scaled-matrix-identity-2-by-2 a) (list a 0 0 a))
(define a '(1 0 0 1))
(define b '(2 0 0 2))
(define x '(1 2 3 4))
(equal? '(7 10 15 22) (*matrix-2-by-2 '(1 2 3 4) '(1 2 3 4)))
(linear-combination +matrix-2-by-2 *matrix-2-by-2 a b x x)

(define test-linear-combination-asm
  (begin (reset-label-counter)
         (compile
          '(begin
             (define (cadr x) (car (cdr x)))
             (define (caddr x) (car (cdr (cdr x))))
             (define (cadddr x) (car (cdr (cdr (cdr x)))))
             (define (linear-combination + * a b x y)
               (+ (* a x) (* b y)))
             (define (+matrix-2-by-2 A B)
               (list (+ (car A) (car B))
                     (+ (cadr A) (cadr B))
                     (+ (caddr A) (caddr B))
                     (+ (cadddr A) (cadddr B))))
             (define (*matrix-2-by-2 A B)
               (list (+ (* (car A) (car B)) (* (cadr A) (caddr B)))
                     (+ (* (car A) (cadr B)) (* (cadr A) (cadddr B)))
                     (+ (* (caddr A) (car B)) (* (cadddr A) (caddr B)))
                     (+ (* (caddr A) (cadr B)) (* (cadddr A) (cadddr B)))))
             (define a '(1 0 0 1))
             (define b '(2 0 0 2))
             (define x '(1 2 3 4)))
          'val
          'next
          empty-compile-time-environment)))
(print-compiled-instruction-sequence test-linear-combination-asm)


(define (compiled-test-linear-comb-eval-with-monitoring a)
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
                     (define (cadr x) (car (cdr x)))
                     (define (caddr x) (car (cdr (cdr x))))
                     (define (cadddr x) (car (cdr (cdr (cdr x)))))
                     (define (linear-combination + * a b x y)
                       (+ (* a x) (* b y)))
                     (define (+matrix-2-by-2 A B)
                       (list (+ (car A) (car B))
                             (+ (cadr A) (cadr B))
                             (+ (caddr A) (caddr B))
                             (+ (cadddr A) (cadddr B))))
                     (define (*matrix-2-by-2 A B)
                       (list (+ (* (car A) (car B)) (* (cadr A) (caddr B)))
                             (+ (* (car A) (cadr B)) (* (cadr A) (cadddr B)))
                             (+ (* (caddr A) (car B)) (* (cadddr A) (caddr B)))
                             (+ (* (caddr A) (cadr B)) (* (cadddr A) (cadddr B)))))
                     (define (scaled-matrix-identity-2-by-2 a) (list a 0 0 a))
                     (define a '(1 0 0 1))
                     (define b (scaled-matrix-identity-2-by-2 ,a))
                     (define x '(1 2 3 4))
                     (linear-combination +matrix-2-by-2 *matrix-2-by-2 a b x x))
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
(define (interactive-compiled-test-linear-comb-eval-with-monitoring)
  (display ";;; enter a")
  (newline)
  (let ((a (read)))
    (let ((result (compiled-test-linear-comb-eval-with-monitoring a)))
      (newline)
      (display "test-linear-comb of ")
      (display a)
      (display " = ")
      (display result)
      (display "    check value of test-linear-comb = ")
      (display (linear-combination +matrix-2-by-2 *matrix-2-by-2
                                   '(1 0 0 1)
                                   (scaled-matrix-identity-2-by-2 a)
                                   '(1 2 3 4)
                                   '(1 2 3 4)))))
  (newline)
  (interactive-compiled-test-linear-comb-eval-with-monitoring))

;; Test case #7
#|
fib    n    maximum depth    total pushes
0      0    2                2
1      1    2                2
1      2    4                9
2      3    6                16
3      4    8                30
5      5    10               51
8      6    12               86
|#

(define (fib-rec n)
  (if (< n 2)
      n
      (+ (fib-rec (- n 1)) (fib-rec (- n 2)))))
(define fib-tester
  (begin (reset-label-counter)
         (compile
          '(define (fib n)
             (if (< n 2)
                 n
                 (+ (fib (- n 1)) (fib (- n 2)))))
          'val
          'next
          empty-compile-time-environment)))
(print-compiled-instruction-sequence fib-tester)
(define (compiled-fib-eval-with-monitoring n)
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
                     (define (fib n)
                       (if (< n 2)
                           n
                           (+ (fib (- n 1)) (fib (- n 2)))))
                     (fib ,n))
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
(define (interactive-compiled-fib-eval-with-monitoring)
  (display ";;; enter n")
  (newline)
  (let ((n (read)))
    (let ((result (compiled-fib-eval-with-monitoring n)))
      (newline)
      (display "fib of ")
      (display n)
      (display " = ")
      (display result)
      (display "    check falue of fib = ")
      (display (fib-rec n))))
  (newline)
  (interactive-compiled-fib-eval-with-monitoring))

;; Test case #8
(define fib-iter-tester
  (begin (reset-label-counter)
         (compile
          '(define (fib n)
             (define (iter a b n)
               (if (= n 0)
                   a
                   (iter b (+ a b) (- n 1))))
             (iter 0 1 n))
          'val
          'next
          empty-compile-time-environment)))
(print-compiled-instruction-sequence fib-iter-tester)
(define (compiled-fib-iter-eval-with-monitoring n)
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
                     (define (fib n)
                        (define (iter a b n)
                          (if (= n 0)
                              a
                              (iter b (+ a b) (- n 1))))
                        (iter 0 1 n))
                     (fib ,n))
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
(define (interactive-compiled-fib-iter-eval-with-monitoring)
  (display ";;; enter n")
  (newline)
  (let ((n (read)))
    (let ((result (compiled-fib-iter-eval-with-monitoring n)))
      (newline)
      (display "fib-iter of ")
      (display n)
      (display " = ")
      (display result)
      (display "    check falue of fib-iter = ")
      (display (fib-rec n))))
  (newline)
  (interactive-compiled-fib-iter-eval-with-monitoring))

