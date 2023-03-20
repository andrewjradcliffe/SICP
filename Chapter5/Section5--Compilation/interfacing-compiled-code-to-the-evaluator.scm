;; Section 5.5.7 Interfacing Compiled Code to the Evaluator

;; Ex. 5.45 -- factorial
;; a
#|
machine type                maximum depth                number of pushes
special-purpose             2n - 2                       2n - 2
interpreted                 5n + 3                       32n - 16
compiled                    3n - 1                       6n - 4
compiled, open-coding       2n - 2                       2n - 2


ratio: compiled / interpreted

number of pushes
lim n->inf (6n - 4) / (32n - 16) = 3/16

maximum depth
lim n->inf (3n - 1) / (5n + 3)   = 3/5


ratio: special-purpose / interpreted

number of pushes
lim n->inf (2n - 2) / (32n - 16) = 1/16

maximum depth
lim n->inf (2n - 2) / (5n + 3)   = 2/5


ratio of ratios: (special-purpose / interpreted) / (compiled / interpreted)

number of pushes
(1/16) / (3/16) = 1/3

maximum depth
(2/5) / (3/5)   = 2/3


Thus, the special-purpose machine uses 1/3 the time and 2/3 the memory
compared to the compiled machine, taking number of pushes to be proportional to time
and maximum depth proportional to memory.
|#
;; b
#|
Open-coding of primitives, which includes the addition of registers arg1 and arg2,
offers a substantial improvement in the performance of the compiler -- in fact,
it matches the performance of the special-purpose code. This is largely due to
the presence of the added registers, which serve as temporary storage for values
which would otherwise need to be stored on the stack. By addition of registers,
we no longer build up argument lists through the stack-intensive process
of computing a value, consing it onto the argl, saving the argl, computing
the next value targeted to val, restoring the argl, consing it onto argl, etc.,
until we have built up the full argument list required for the procedure application.
In essence, addition of registers to hold arguments for a procedure call greatly
reduces the number of stack allocations by eliminating the dynamic creation of
an argument list. A more sophisticated compiler could apply the analogous
methodology to procedure calls, as the length of their argument lists is known at compile
time (except procedures taking a variable number of arguments).
|#


;; Ex. 5.46 -- Fibonacci
#|
machine type                maximum depth                number of pushes
special-purpose             2n - 2                       3Fib(n + 1) - 3
interpreted                 5n + 3                       56Fib(n + 1) - 40
compiled                    3n - 1                       10Fib(n + 1) - 8
compiled, open-coding       2n                           7Fib(n + 1) - 5


ratio: compiled / interpreted

number of pushes
(10Fib(n + 1) - 8) / (56Fib(n + 1) - 40)

maximum depth
lim n->inf (3n - 1) / (5n + 3)   = 3/5


ratio: special-purpose / interpreted

number of pushes
(3Fib(n + 1) - 3) / (56Fib(n + 1) - 40)

maximum depth
lim n->inf (2n - 2) / (5n + 3)   = 2/5


Though we can express Fib(n) = ⌊ ϕⁿ / √5 ⌉ , ϕ = (1 + √5) / 2,
in the strict sense, ⌊ ϕⁿ / √5 ⌉ does not have a derivative as the nearest-integer
function is discontinuous. Hence, while it is tempting to the the limit as n->inf
for the ratio of pushes, it is not mathematically valid as the derivative does not
exist for Fib(n) -- the sequence of ratios does not converge, but rather, oscillates;
the range of oscillation may not be particularly wide, but, nonetheless, it does not
actually converge in the usual sense.

However, we can examine the ratios of coefficients to obtain some insight.
The compiled code uses approximately 10/56 the time resource of the interpreted code,
and the special-purpose code approximately 3/56 of the time resource.
Open-coding of primitives in compiled code brings us to approximately 7/56,
but we do not reach the special purpose machine's efficiency as we are still
using save/restore around non-primitive procedure calls.
Nonetheless, compilation (with our simple compilers) realizes significant gains;
a more sophisticated compiler could likely generate the same code as the special-purpose
machine by explicitly allocating registers.
|#



;; Ex. 5.47
(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (interpreted-branch (make-label 'interpreted-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
                                  `((test (op primitive-procedure?) (reg proc))
                                    (branch (label ,primitive-branch))))
       (make-instruction-sequence '(proc) '()
                                  `((test (op compound-procedure?) (reg proc))
                                    (branch (label ,interpreted-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage))
        (parallel-instruction-sequences
         (append-instruction-sequences
          interpreted-branch
          (compile-interpreted-proc-appl target compiled-linkage))
         (append-instruction-sequences
          primitive-branch
          (end-with-linkage linkage
                            (make-instruction-sequence '(proc argl) (list target)
                                                       `((assign ,target
                                                                 (op apply-primitive-procedure)
                                                                 (reg proc)
                                                                 (reg argl))))))))
       after-call))))

#|
compound-apply sets up the registers for ev-sequence, then proceeds into ev-sequence;
at the last step of ev-sequence, continue is restored. Thus, we need to save continue
so that we return the appropriate place (as continue is assigned within ev-sequence).
|#
(define (compile-interpreted-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
                                    `((assign continue (label ,linkage))
                                      (save continue)
                                      (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
                                      `((assign continue (label ,proc-return))
                                        (save continue)
                                        (goto (reg compapp))
                                        ,proc-return
                                        (assign ,target (reg val))
                                        (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc) all-regs
                                    '((save continue)
                                      (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))

;; Ex. 5.48
(define (compile-and-run expression)
  (let ((instructions
         (assemble (statements
                    (compile (exp-to-compile expression) 'val 'return))
                   eceval)))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))
(define (compile-and-run? exp) (tagged-list? exp 'compile-and-run))
(define (exp-to-compile exp) (cadr exp))
;; within eval-dispatch, prior to application?
(test (op compile-and-run?) (reg exp))
(branch (label ev-compile-and-run))
;; at end of eceval-controller
ev-compile-and-run
(perform (op compile-and-run) (reg exp))
