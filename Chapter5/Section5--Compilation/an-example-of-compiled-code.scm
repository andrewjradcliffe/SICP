;; 5.5.5 An Example of Compiled Code

(define (print-instruction instruction)
  (newline)
  (if (symbol? instruction)
      (display instruction)
      (begin (display "  ")
             (display instruction))))
(define (print-compiled-instruction-sequence seq)
  (for-each print-instruction (statements seq)))

;; Ex. 5.33
;; Figure 5.17's code
(define figure-5.17
  (begin (reset-label-counter)
         (compile
          '(define (factorial n)
             (if (= n 1)
                 1
                 (* (factorial (- n 1)) n)))
          'val
          'next)))
(print-compiled-instruction-sequence figure-5.17)

(define factorial-alt
  (begin (reset-label-counter)
         (compile
          '(define (factorial-alt n)
             (if (= n 1)
                 1
                 (* n (factorial-alt (- n 1)))))
          'val
          'next)))
(print-compiled-instruction-sequence factorial-alt)

;; Differences
#|
factorial-alt requires a save/restore of the env during evaluation of the
operands, as (factorial (- n 1)) will create an extended environment, but n must
be available to be looked up (i.e. in the environment in which * is called).

This does not result in any additional save/restores, as the argl does not
need to be save/restore'd in factorial-alt -- once we have computed (factorial (- n 1)),
we go directly to a variable lookup, which can simply be cons'd onto the existing argl.
|#

;; Efficiency of execution
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/compiled-code-evaluator.scm")
#|
As demonstrated below, there is no difference in efficiency in terms of
maximum depth and number of pushes.

factorial
_________

n                maximum depth                number of pushes
1                2                            2
2                5                            8
3                8                            14
4                11                           20
5                14                           26
6                17                           32


factorial-alt
_____________

n                maximum depth                number of pushes
1                2                            2
2                5                            8
3                8                            14
4                11                           20
5                14                           26
6                17                           32

|#
(define (compiled-factorial-eval-with-monitoring n)
  (define compiled-machine
    (make-machine
     all-regs
     compiled-code-operations
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
                  'next)
                 ))
       (perform (op print-stack-statistics))
       )
     ))
  (define the-global-environment (setup-environment))
  (define (get-global-environment) the-global-environment)
  (set-register-contents! compiled-machine 'env (get-global-environment))
  (start compiled-machine)
  (get-register-contents compiled-machine 'val)
  )

(compiled-factorial-eval-with-monitoring 1)
(compiled-factorial-eval-with-monitoring 2)
(compiled-factorial-eval-with-monitoring 3)
(compiled-factorial-eval-with-monitoring 4)
(compiled-factorial-eval-with-monitoring 5)
(compiled-factorial-eval-with-monitoring 6)


(define (compiled-factorial-alt-eval-with-monitoring n)
  (define compiled-machine
    (make-machine
     all-regs
     compiled-code-operations
     `(
       (perform (op initialize-stack))
       ,@(statements
          (begin (reset-label-counter)
                 (compile
                  `(begin
                     (define (factorial-alt n)
                       (if (= n 1)
                           1
                           (* n (factorial-alt (- n 1)))))
                     (factorial-alt ,n))
                  'val
                  'next)
                 ))
       (perform (op print-stack-statistics))
       )
     ))
  (define the-global-environment (setup-environment))
  (define (get-global-environment) the-global-environment)
  (set-register-contents! compiled-machine 'env (get-global-environment))
  (start compiled-machine)
  (get-register-contents compiled-machine 'val)
  )

(compiled-factorial-alt-eval-with-monitoring 1)
(compiled-factorial-alt-eval-with-monitoring 2)
(compiled-factorial-alt-eval-with-monitoring 3)
(compiled-factorial-alt-eval-with-monitoring 4)
(compiled-factorial-alt-eval-with-monitoring 5)
(compiled-factorial-alt-eval-with-monitoring 6)

