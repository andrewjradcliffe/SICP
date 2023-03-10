;; 5.4.4 Running the Evaluator

;; Ex. 5.26
(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;; a
#|
maximum depth is 10
|#
(factorial 1)
(factorial 2)
(factorial 3)
(factorial 4)


;; b
#|
8 push per (> ...)
3 push per if
8 push per (* ...)
8 push per (+ ...)
8 push per (iter ...)


iter call, consequent branch taken: 19 (11 internal)
8 (iter ...)
3 if
8 (> ...)

iter call, alternative branch taken: 35 (27 internal)
8 (iter ...)
3 if
8 (> ...)
8 (* ...)
8 (+ ...)

n = 0, (iter 1 1) : 8 + 11
n = 1, (iter 1 1) : 8 + 27 + 8 + 11
n = 2, (iter 1 1) : 8 + 27 + 8 + 27 + 8 + 11

n + 1 (iter ...)           = 8(n + 1)
n alternative branch       = 27n
1 consequent branch        = 11

Thus, 35n + 19 from iter, and 10 from factorial body


|#
(define (iter-pushes n) (+ 19 (* 35 n)))
(define factorial-body-pushes 10)
(define (total-pushes n) (+ factorial-body-pushes (iter-pushes n)))


;; Ex. 5.27
(define (factorial n)
  (if (= n 1)
      1
      (* (factorial (- n 1)) n)))
;; b
#|
5 push per (factorial <literal>)
3 push per if
8 push per (= ...)
8 push per (- ...)
8 push per (* ...)

factorial call, consequent branch: 16 (11 internal)
5 (factorial ...)
3 if
8 (= ...)

factorial call, alternative branch: 32 (27 internal)
5 (factorial ...)
3 if
8 (= ...)
8 (* ...)
8 (- ...)

n = 1, (factorial 1) : 5 + 3 + 8
n = 2, (factorial 2) : 5 + 3 + 8 + 8 +  (5 + 3 + 8)  + 8
                                   ^~   ^~~~~~~~~~~    ^~
                                  (-)  (factorial 1)  (*)
n = 3, (factorial 3) : 5 + 3 + 8 + 8 + (5 + 3 + 8 + 8 +  (5 + 3 + 8)   + 8) + 8
                                   ^~               ^~   ^~~~~~~~~~~     ^~   ^~
                                  (-)              (-)  (factorial 1)   (*)  (*)
                                       ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                                        (factorial 2)

n (factorial <literal>)       = 5n
n if                          = 3n
n (= ...)                     = 8n
n - 1 (- ...)                 = 8(n - 1)
n - 1 (* ...)                 = 8(n - 1)

Thus, 32n - 16 total pushes
|#
(define (total-pushes-rec n) (- (* 32 n) 16))

;; a
#|
(- n 1) : has a maximum depth of 5
(factorial <literal>) : has a maximum depth of 8 prior to reaching eval-if-decide
(factorial (- n 1)) : at the point at which (- n 1) is evaluated, stack has a depth of 3.
Thus, in order to construct the argument, a depth of 8 is reached; it then contracts to 3.

At the point one reaches (factorial (- n 1)), the stack has depth 5.

n = 1, (factorial 1) : 8
n = 2, (factorial 2) : 5 + 8
n = 3, (factorial 3) : 5 + 5 + 8

5(n - 1) + 8 = 5n + 3
|#
(define (maximum-depth-rec n) (+ 3 (* 5 n)))

#|
                        Maximum depth                 Number of pushes
Recursive factorial     5n + 3                        32n - 16
Iterative factorial     10                            35n + 19 + 10
|#


;; Ex. 5.30

;; a
#|
There are several changes required to gracefully handle errors. Below, I cover
two: unbound variables and incorrect number of arguments with compound procedures.
|#

;; unbound variables
#|
Strategy: upon failing to find a binding for a variable, assign '*unbound*
Then, everywhere val is read, perform a check to determine if the val is '*unbound*,
and if so, branch to unbound-variable entry point.
|#
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        ''*unbound*
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define unbound-variable-entry-point
  '(unbound-variable
    (assign val (const unbound-variable-error))
    (goto (label signal-error))))
(define (unbound-variable? val) (eq? val '*unbound*))

#|
It is tempting to immediately check for this within ev-variable (in which case we could
(restore continue) within the unbound-variable entry point itself).

More rigorously, there are 6 locations at which we must insert a test and branch:
ev-appl-did-operator
ev-appl-accumulate-arg
ev-appl-accum-last-arg
ev-if-decide
ev-assignment-1
ev-definition-1

Prior to the reading of val in these locations, one must insert:
(test (op unbound-variable?) (reg val))
(branch (label unbound-variable))

ev-assignment-1 needs special handling, as set-variable-value! must also create
the same type of error when the binding simply does not exist.
|#
(define revised-ev-assignment-1
  '(ev-assignment-1
    (restore continue)
    (restore env)
    (restore unev)
    (test (op unbound-variable?) (reg val))
    (branch (label unbound-variable))
    (save val)
    (assign val (op lookup-variable-value) (reg unev) (reg env))
    (test (op unbound-variable?) (reg val))
    (branch (label unbound-variable-assignment))
    (restore val)
    (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
    (assign val (const ok))
    (goto (reg continue))
    unbound-variable-assignment
    (restore val) ;; cleanup stack
    (goto (label unbound-variable))
    ))

;; compound procedure application: the number of parameters must match the number of arguments.
(define (same-length? x y) (= (length x) (length y)))
(define revised-compound-apply
  '(compound-apply
    (assign unev (op procedure-parameters) (reg proc))
    (test (op same-length?) (reg unev) (reg argl))
    (branch (label unequal-parameters-arguments))
    (assign env (op procedure-environment) (reg proc))
    (assign env (op extend-environment)
            (reg unev) (reg argl) (reg env))
    (assign unev (op procedure-body) (reg proc))
    (goto (label ev-sequence))
    ))


;; b
#|
Example for car, cdr, /. A more robust strategy could be used which would spare
the system creator the burden of defining safe- operations, but this is
just a simple approach.

For the sake of efficiency, the application of primitive procedure always "succeeds",
but the val may be an error code. We simultaneously set the contents of a distinct
register -- condition-flag, which we add to the machine -- and then test the register
after application of the primitive procedure to determine whether the value stored in
the val register is not an error. If it is an en error, we branch to signal-error.
This allows each primitive operation to return its own error code (however its
implementer desires), and to clearly specify success/failure of the primitive application.

Moreover, this allows the implementer of the evaluator to not need to worry whether
any particular value stored in val is OK or not -- that information is available by
simply inspecting condition-flag.

|#
(define (apply-primitive-procedure proc argl condition-flag)
  (apply (safe-procedure proc) argl))
(define (set-register-true! register)
  (set-contents! register true))
(define (set-register-false! register)
  (set-contents! register false))

(define (safe-procedure proc)
  (get proc))
(define (safe-car x)
  (if (pair? x)
      (begin (set-register-false! condition-flag)
             (car x))
      (begin (set-register-true! condition-flag)
             'bad-car)))
(define (safe-cdr x)
  (if (pair? x)
      (begin (set-register-false! condition-flag)
             (cdr x))
      (begin (set-register-true! condition-flag)
             'bad-cdr)))
(define (safe-/ x y)
  (if (= y 0)
      (begin (set-register-true! condition-flag)
             'div-by-zero)
      (begin (set-register-false! condition-flag)
             (/ x y))))

(put car safe-car)
(put cdr safe-cdr)
(put / safe-/)

;; Then, update primitive-apply.
(define revised-primitive-apply
  '(primitive-apply
    (assign val (op apply-primitive-procedure)
            (reg proc)
            (reg argl)
            (reg condition-flag))
    (test (op true?) (reg condition-flag))
    (branch (label signal-error))
    (restore continue)
    (goto (reg continue))))

;; This captures the essence of a generic safe operation. proc, test, and message can
;; be tailored to the needs of the primitive operation.
(define (safe-op proc test message)
  (lambda (args)
    (if (test args)
        (begin (set-register-false! condition-flag)
               (proc args))
        (begin (set-register-true! condition-flag)
               message))))

;; In fact, depending on the interface we want to provide to other implementers,
;; we could decline to  "apply" within apply-primitive-procedure itself,
;; and use the convention that test and proc will receive and argument list.
;; This allows each primitive function to destructure the argument list in whatever way
;; it desires -- frequently apply, but, by receiving the arg list, one can test that
;; the correct number of arguments have been supplied.
(define (extra-safe-/ args)
  (if (= (length args) 2)
      (if (not (= (cadr args) 0))
          (/ (car args) (cadr args))
          'div-by-zero)
      'div-too-many-arguments))

(define (safe-div-test args)
  (and (= (length args) 2) (not (= (cadr args) 0))))
(define (safe-div-proc args)
  (/ (car args) (cadr args)))
(define safe-div-message 'bad-div)

(put / (safe-op safe-div-proc safe-div-test safe-div-message))

(define (safe-apply-primitive-procedure proc argl condition-flag)
  ((safe-procedure proc) argl))
