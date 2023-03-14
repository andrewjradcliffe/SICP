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

formula for maximum depth: 3(n-1) + 2
formula for number of pushes: 6(n-1) + 2

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
(define (interactive-compiled-factorial-eval-with-monitoring)
  (let ((n (read)))
    (compiled-factorial-eval-with-monitoring n))
  (newline)
  (interactive-compiled-factorial-eval-with-monitoring))
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



;; Ex. 5.34
#|
Mechanism of action

The difference between the recursive and iterative code is that
the iterative code stores its essential state in argl -- which we can think of
as two registers that are continuously re-used -- then simply jumps to the entry
point of the next call. When we jump, there is no state to store other than
that already contained in argl.

On the other hand, the recursive code builds up a stack which contains partial
evaluations of procedures, ending the build-up only when it reaches the bottom.

In essence, the iterative procedure never needs to save a partial evaluation --
it always evaluates to completion, hence, there is no need to store state associated
with an evaluation-in-progress.

The stack is used only to store the temporaries necessary for preservation of
the procedure, environment and continue during
the evaluation of (+ counter 1) and (* counter product) -- which requires that
we keep argl containing (+ counter 1) on the stack temporarily.
When we enter the next call of iter, we have nothing on the stack. Thus,
we can view this as a process which adds things to the stack (up to a maximum of 3),
then clears the stack prior to proceeding.

In contrast, the recursive factorial does not clear the stack prior to entering
the next factorial call (it cannot due to the need to save continue, env and argl
from the preceding call).

|#
(define factorial-iter
  (begin (reset-label-counter)
         (compile
          '(define (factorial n)
             (define (iter product counter)
               (if (> counter n)
                   product
                   (iter (* counter product)
                         (+ counter 1))))
             (iter 1 1))
          'val
          'next)))
(print-compiled-instruction-sequence factorial-iter)

;; Efficiency of execution
#|
factorial-iter
_____________

n                maximum depth                number of pushes
1                3                            8
2                3                            14
3                3                            20
4                3                            26
5                3                            32
6                3                            38

|#
(define (compiled-factorial-iter-eval-with-monitoring n)
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
                       (define (iter product counter)
                         (if (> counter n)
                             product
                             (iter (* counter product)
                                   (+ counter 1))))
                       (iter 1 1))
                     (factorial ,n))
                  'val
                  'next)))
       (perform (op print-stack-statistics))
       )
     ))
  (define the-global-environment (setup-environment))
  (define (get-global-environment) the-global-environment)
  (set-register-contents! compiled-machine 'env (get-global-environment))
  (start compiled-machine)
  (get-register-contents compiled-machine 'val)
  )

(compiled-factorial-iter-eval-with-monitoring 1)
(compiled-factorial-iter-eval-with-monitoring 2)
(compiled-factorial-iter-eval-with-monitoring 3)
(compiled-factorial-iter-eval-with-monitoring 4)
(compiled-factorial-iter-eval-with-monitoring 5)
(compiled-factorial-iter-eval-with-monitoring 6)


;; Ex. 5.35
(define f-5.35
  (begin
    (set! label-counter 14)
    (compile
     '(define (f x)
        (+ x (g (+ x 2))))
     'val
     'next)))
(print-compiled-instruction-sequence f-5.35)


;; Ex. 5.36
#|
The compiler produces right-to-left evaluation order. This is determined in
construct-arglist.

A very simple way to produce left-to-right evaluation order is to remove the
reverse from construct-arglist, thereby generating an argl which is the revserse
of that required by the procedure, then using reverse as a primitive operation
on the argl to fix the situation. I denote this below as Version 1.

The other option remove the reverse from construct-arglist, and change the cons
within construct-arglist to adjoint-arg; this is less invasive, and more agreeable
from the perspective of primitive operations.

Efficiency of right-to-left:     𝒪(n) cons operations (exactly n for n arguments)

Efficiency of left-to-right:     𝒪(n²) cons, car, cdr, null? operations (yes, 𝒪(n²) for each!)

Breakdown of left-to-right
__________________________

1 list operation
n - 1 adjoin-arg operations    =>    n - 1 list operations    =>     n - 1 cons
                                     n - 1 append operations  =>     see below

(define (append seq1 seq2)
  (if (null? seq1)
      seq1
      (cons (car seq1)
            (append (cdr seq1) seq2))))


if-predicate: 1 null?
if-consequent: nothing
if-alternative: 1 cons, 1 car, 1 cdr

Given seq1 of length n:    n + 1 null?
                           n cons
                           n car
                           n cdr

Thus, we have:
cons, car, cdr    =>    ∑ᵢ₌₁ⁿ⁻¹ i
null?             =>    ∑ᵢ₌₁ⁿ⁻¹ i+1

Recall that: ∑ᵢ₌₁ⁿ⁻¹ i = n * (n + 1) / 2

∴
append:                    (n - 1) * n / 2                 cons, car, cdr
                           (n - 1) * n / 2 + n - 1         null?
list within adjoin-arg:    n - 1                           cons
list for first arg:        1                               cons

Hence, 𝒪(n²) complexity for both time and space.

The efficiency of the code that constructs argument lists suffers tremendously --
now, rather than being linear in the number of arguments, it is quadratic.
Thus, code that has a greater number of arguments per procedure will be
exponentially slower.
|#

;; Version 1
(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence '(val argl) '(argl)
                                                ((assign argl
                                                         (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        (append-instruction-sequences
         code-for-next-arg
         (make-instruction-sequence '(argl) '(argl)
                                    '((assign argl
                                              (op reverse) (reg argl)))))
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))


;; Ex. 5.37
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (preserving (cdr regs)
                    (make-instruction-sequence
                     (list-union (list first-reg)
                                 (registers-needed seq1))
                     (list-difference (registers-modified seq1)
                                      (list first-reg))
                     (append `((save ,first-reg))
                             (statements seq1)
                             `((restore ,first-reg))))
                    seq2))))


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

;; within compile, prior to application?
((application-open-code? exp)
 (compile-open-code exp target linkage))


;; c
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/compiler-with-ex.5.38.scm")
(define (compiled-factorial-eval-with-monitoring n)
  (define compiled-machine
    (make-machine
     `(,@all-regs arg1 arg2)
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
(define (interactive-compiled-factorial-eval-with-monitoring)
  (let ((n (read)))
    (compiled-factorial-eval-with-monitoring n))
  (newline)
  (interactive-compiled-factorial-eval-with-monitoring))
