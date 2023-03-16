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

;; all-regs should be re-defined to include arg1, arg2
(define all-regs '(env proc val argl continue arg1 arg2))

;; a
;; (define (spread-arguments operands-list)
;;   (let ((op-code-2
;;          (append-instruction-sequences
;;           (compile (cadr operands-list) 'val 'next)
;;           (make-instruction-sequence '(val) '(arg2)
;;                                      '((assign arg2 (reg val)))))))
;;     (preserving '(env)
;;                 op-code-2
;;                 (preserving '(arg2)
;;                             (compile (car operands-list) 'val 'next)
;;                             (make-instruction-sequence '(val arg2) '(arg1)
;;                                                        '((assign arg1 (reg val))))))))

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

;; within compile, prior to application?
((application-open-code? exp)
 (compile-open-code exp target linkage))


;; c
#|
factorial -- with open-coding of primitives
_________

n                maximum depth                number of pushes
1                0                            0
2                2                            2
3                4                            4
4                6                            6
5                8                            8
6                10                           10

Dramatic increase in efficiency.
|#
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/compiler-with-ex.5.38.scm")
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

;; A useful tester
(define (compiled-multi-adder-with-monitoring adder-exp a b c d)
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
                     (define (multi-adder a b c d)
                       ,adder-exp)
                     (multi-adder ,a ,b ,c ,d))
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
  (get-register-contents compiled-machine 'val))
(define (interactive-compiled-multi-adder-with-monitoring adder-exp)
  (display ";;; enter (a b c d)")
  (newline)
  (let ((abcd (read)))
    (let ((a (car abcd))
          (b (cadr abcd))
          (c (caddr abcd))
          (d (cadddr abcd)))
      (let ((result (compiled-multi-adder-with-monitoring adder-exp a b c d)))
        (newline)
        (display "result = ")
        (display result))))
  (newline)
  (interactive-compiled-multi-adder-with-monitoring adder-exp))
(define right-to-left '(+ a (+ b (+ c d))))
(define left-to-right '(+ (+ (+ a b) c) d))
(define mixed '(+ (+ a b) (+ c d)))
(define nested '(+ (+ (+ a (+ a (+ b (+ c d)))) c) d))
(define varargs '(+ a b c d))

(define (print-multi-adder-assembly adder-exp)
  (define multi-adder
    (begin (reset-label-counter)
           (compile
            `(define (multi-adder a b c d)
               ,adder-exp)
            'val
            'next)))
  (print-compiled-instruction-sequence multi-adder))
(print-multi-adder-assembly right-to-left)
(print-multi-adder-assembly left-to-right)
(print-multi-adder-assembly mixed)
(print-multi-adder-assembly varargs)


;; Another useful tester; multiple compiled procedure calls prior to completion
;; of an open-coded primitive (+).
(define (compiled-fib-eval-with-monitoring n)
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
                     (define (fib n)
                       (if (< n 2)
                           n
                           (+ (fib (- n 1)) (fib (- n 2)))))
                     (fib ,n))
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
(define (interactive-compiled-fib-eval-with-monitoring)
  (display ";;; enter n")
  (newline)
  (let ((n (read)))
    (let ((result (compiled-fib-eval-with-monitoring n)))
      (newline)
      (display "fib of ")
      (display n)
      (display " = ")
      (display result)))
  (newline)
  (interactive-compiled-fib-eval-with-monitoring))
(define fib-tester
  (begin (reset-label-counter)
         (compile
          '(define (fib n)
             (if (< n 2)
                 n
                 (+ (fib (- n 1)) (fib (- n 2)))))
          'val
          'next)))
(print-compiled-instruction-sequence fib-tester)




;; d
#|
There are two approaches: construct directly (denoted as Version 1), or,
a syntactic transformation (denoted as Version 2).

Version 2 seems to have a disadvantage -- it should incur more stack allocations
than Version 1.

Version 2 is the least complicated way to implement varargs, and also the most robust.
Notably, comparison of the simplified Version 1 and Version 2 indicates that they
produce identical machine code; hence, it is preferable to just use a syntactic
transformation, rather than to add a specific code generator (which is useless).

Version 2 incurs no more stack allocations than the simplified Version 1.
The presence or absence of stack allocations in varargs statements depends on
the folding direction chosen for the implementation of two-argument open-coded
primitives -- if one chooses fold-left, then one gets stack allocations; if
one chooses fold-right, then nothing needs to be saved on the stack.

Consider an expression: (+ a b c)
which corresponds to: (+ a (+ b c))
    or, equivalently: (+ (+ a b) c)

As our two-argument open-coded primitive implementation folds from right-to-left,
the latter incurs a stack allocation. If we choose to implement open-coding which
folds from left-to-right, then the former incurs a stack allocation.
In essence, we need to implement varargs handling in the manner which matches
the direction of folding used for our two-argument open-coding implementation.

If we were to write a more sophisticated compiler, then we could eliminate
some of these stack operations by analyzing the source program and re-arranging
expressions involving open-coded primitives to conform to our folding direction.
It would be legal to enact such a change everywhere, except for floating point
operations (since floating point arithmetic is not associative).

Interestingly, even for programming languages without varargs overloading of
basic operators, this implies that each compiler would have a preferred
order for nesting of parentheses in floating point arithmetic. If one matches
the order, then one can re-use the same two registers; if not, then one would
incur stack allocations -- though, a compiler would most likely allocate a register
for each output in a sequence, rather than push/pop from the stack; but, the
allocated registers would have to come from the language runtime, thus it actually
would amount to stack operations somewhere.

|#
(define (application-open-code-varargs? exp)
  (if (pair? exp)
      (let ((op (car exp)))
        (and (or (eq? op '+) (eq? op '*))
             (not (= (length (operands exp)) 2))))
      false))



;; Version 1 -- does not quite work; the simplified version does, however.
;; (define (spread-var-arguments op operands-list)
;;   (let ((operands-list (reverse operands-list)))
;;     (let ((last-code
;;            (append-instruction-sequences
;;             (compile (car operands-list) 'val 'next)
;;             (make-instruction-sequence '(val) '(arg2)
;;                                        '((assign arg2 (reg val)))))))
;;       (preserving '(env)
;;                   last-code
;;                   (spread-rest-args op (cdr operands-list))))))

;; (define (spread-rest-args op operands-list)
;;   (let ((op-code-1
;;          (preserving '(arg2)
;;                      (compile (car operands-list) 'val 'next)
;;                      (make-instruction-sequence '(val arg2) '(arg1)
;;                                                 '((assign arg1 (reg val)))))))
;;     (if (null? (cdr operands-list))
;;         (append-instruction-sequences
;;          op-code-1
;;          (make-instruction-sequence '(arg1 arg2) '(val)
;;                                     `((assign val (op ,op) (reg arg1) (reg arg2)))))
;;         (preserving '(env)
;;                     (append-instruction-sequences
;;                      op-code-1
;;                      (make-instruction-sequence '(arg1 arg2) '(arg2)
;;                                                 `((assign arg2 (op ,op) (reg arg1) (reg arg2)))))
;;                     (spread-rest-args op (cdr operands-list))))))

;; (define (compile-open-code-varargs exp target linkage)
;;   (let ((op (operator exp)))
;;     (let ((argument-code (spread-var-arguments op (operands exp))))
;;       (preserving '(env continue)
;;                   argument-code
;;                   (end-with-linkage linkage
;;                                     (make-instruction-sequence '(val) (list target)
;;                                                                `((assign ,target (reg val)))))
;;                   ;; Or, a smarter ending:
;;                   ;; (if (eq? target 'val)
;;                   ;;     (end-with-linkage linkage (empty-instruction-sequence))
;;                   ;;     (end-with-linkage linkage
;;                   ;;                       (make-instruction-sequence '(val) (list target)
;;                   ;;                                                  `((assign ,target (reg val))))))
;;                   ))))
;; within compile, prior to application-open-code?
((application-open-code-varargs? exp)
 (compile-open-code-varargs exp target linkage))


;; Version 2
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


;; within compile, prior to application-open-code?
((application-open-code-varargs? exp)
 (compile-open-code-varargs exp target linkage))


;; Version 1, revised in light of simplified a
(define (spread-var-arguments op operands-list target)
  (let ((operands-list (reverse operands-list)))
    (let ((last-code (compile (car operands-list) 'arg2 'next)))
      (preserving '(env)
                  last-code
                  (spread-rest-args op (cdr operands-list) target)))))

(define (spread-rest-args op operands-list target)
  (let ((op-code-1
         (preserving '(arg2)
                     (compile (car operands-list) 'arg1 'next)
                     (make-instruction-sequence '(arg2) '() '()))))
    (if (null? (cdr operands-list))
        (append-instruction-sequences
         op-code-1
         (make-instruction-sequence '(arg1 arg2) (list target)
                                    `((assign ,target (op ,op) (reg arg1) (reg arg2)))))
        (preserving '(env)
                    (append-instruction-sequences
                     op-code-1
                     (make-instruction-sequence '(arg1 arg2) '(arg2)
                                                `((assign arg2 (op ,op) (reg arg1) (reg arg2)))))
                    (spread-rest-args op (cdr operands-list) target)))))
(define (neutral-element op)
  (cond ((eq? op '+) 0)
        ((eq? op '*) 1)
        ((eq? op '=) true)
        ((eq? op '>) true)
        ((eq? op '<) true)
        ((eq? op '>=) true)
        ((eq? op '<=) true)
        (else (error "unknown op -- NEUTRAL-ELEMENT" op))))

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
           (preserving '(env continue)
                       (spread-var-arguments op operands-list target)
                       (end-with-linkage linkage
                                         (empty-instruction-sequence)))))))
