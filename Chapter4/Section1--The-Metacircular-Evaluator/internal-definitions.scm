;; 4.1.6 Internal Definitions

;; Ex. 4.16

;; a
;; not using the abstractions from Ex. 4.12
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; b

;; Version 1.1: in forward order, the long way
(define (scan-out-defines exps)
  (define (partition defines regulars exps)
    (if (null? exps)
        (cons defines regulars)
        (let ((first (car exps))
              (rest (cdr exps)))
          (if (definition? first)
              (partition (append defines (list first)) regulars rest)
              (partition defines (append regulars (list first)) rest)))))
  (let ((defines-regulars (partition '() '() exps)))
    (let ((defines (car defines-regulars))
          (regulars (cdr defines-regulars)))
      (if (null? defines)
          exps ;; or, regulars, which is equivalent
          (let ((vars (map definition-variable defines))
                (vals (map definition-value defines)))
            (make-let (map (lambda (x) (list x '*unassigned*)) vars)
                      (append (map (lambda (x y) (list 'set! x y)) vars vals)
                              regulars)))))))


;; Version 1.2: in forward order, the short way
(define (scan-out-defines exps)
  (let ((defines (filter definition? exps))
        (regulars (filter (lambda (x) (not (definition? x))) exps)))
    (if (null? defines)
        exps ;; or, regulars, which is equivalent
        (let ((vars (map definition-variable defines))
              (vals (map definition-value defines)))
          (make-let (map (lambda (x) (list x '*unassigned*)) vars)
                    (append (map (lambda (x y) (list 'set! x y)) vars vals)
                            regulars))))))

;; Version 2: in reverse order, the long way
(define (scan-out-defines exps)
  (define (partition defines regulars exps)
    ;; Note cons: creates defines, regulars in reverse
    (if (null? exps)
        (cons defines regulars)
        (let ((first (car exps))
              (rest (cdr exps)))
          (if (definition? first)
              (partition (cons first defines) regulars rest)
              (partition defines (cons first regulars) rest)))))
  (define (iter defines bindings exps)
    (if (null? defines)
        (make-let bindings exps)
        (let ((var (definition-variable (car defines)))
              (val (definition-value (car defines))))
          (iter (cdr defines)
                (cons (list var '*unassigned*) bindings)
                (cons (list 'set! var val) exps)))))
  (let ((defines-regulars (partition '() '() exps)))
    (let ((defines (car defines-regulars))
          (regulars (cdr defines-regulars)))
      (if (null? defines)
          exps ;; or, regulars, which is equivalent
          (iter defines '() (reverse regulars))))))

;; Clearly, Version 1.2 achieves the best clarity of intent, though,
;; it is the least efficient. Admittedly, since we are going to use analysis,
;; efficiency does not make much of a difference. Besides -- if we really
;; wanted to optimize, we could construct everything in one pass, but it would
;; be quite a mess.


;; Version 3: everything together. As it happens, it is actually not quite as messy
;; as anticipated.
(define (scan-out-defines exps)
  (define (iter bindings set!-exps body-exps exps)
    (if (null? exps)
        (if (null? bindings)
            body-exps
            (make-let bindings (append set!-exps body-exps)))
        (let ((first (car exps))
              (rest (cdr exps)))
          (if (definition? first)
              (let ((var (definition-variable first))
                    (val (definition-value first)))
                (iter (append bindings (list var '*unassigned*))
                      (append set!-exps (list 'set! var val))
                      body-exps
                      rest))
              (iter bindings set!-exps (append body-exps (list first)) rest)))))
  (iter '() '() '() exps))


;; c

;; If in make-procedure:
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;; If in procedure-body:
(define (procedure-body p) (scan-out-defines (caddr p)))

;; From an efficiency perspective, placing this in make-procedure is preferable,
;; as the defines are scanned out a single time -- when the procedure is created.
;; On the other hand, if placed in procedure-body, each time the procedure is
;; applied, the defines will be scanned out. Clearly, the latter is far less efficient.


;; Ex. 4.17

;; (lambda <vars>
;;   (define u <e1>)
;;   (define v <e2>)
;;   <e3>)

#|
When evaluated, creates a procedure object: ('procedure <vars> <body> '<env-of-lambda>)
<e3> will be evaluated when the procedure is applied, e.g. (proc <args>), which
will cause the procedure to be evaluated as a compound procedure,
which involves a sequential evaluation of the expressions in the body in an environment
(extended from the environment of the procedure) in which the arguments are bound to
the variables.

                                 env-of-lambda
                                    ^
                                    |
                                    |
                        +------------------+
                        | <vars> : <args>  |
                        |                  |
extended-env  --------->|                  |
                        |                  |
                        |                  |
                        +------------------+

The first define statement adds u: result of evaluating e1 in extended-env
The second define statement adds v: result of evaluating e2 in extended-env
Evaluation of e3 occurs in an environment (extended-env) in which <vars>, u, v are bound.
|#

;; (lambda <vars>
;;   (let ((u '*unassigned*)
;;         (v '*unassigned*))
;;     (set! u <e1>)
;;     (set! v <e2>)
;;     <e3>))

#|
When evaluated, creates a procedure object. The procedure is then called on some args,
resulting in the creation of an extended environment in which the vars are bound to args.

                                 env-of-lambda
                                    ^
                                    |
                                    |
                        +------------------+
                        | <vars> : <args>  |
                        |                  |
extended-env  --------->|                  |
                        |                  |
                        |                  |
                        +------------------+

Evaluation of the single body expression results in a lambda being created
and called on '*unassigned* '*unassigned*

 (lambda (u v)
   (set! u <e1>)
   (set! v <e2>)
   <e3>)

When the lambda is evaluated, a procedure is generated, the environment part of which
is extended-env. Then, when this procedure is called on '*unassigned* '*unassigned*,
a new extended environment is created:

                                 extended-env
                                    ^
                                    |
                                    |
                        +------------------+
                        | u: '*unassigned* |
                        | v: '*unassigned* |
   extra-env  --------->|                  |
                        |                  |
                        |                  |
                        +------------------+

<e1> is then evaluated in this environment and u is set! to the value returned;
likewise for <e2>, v. Any references to <vars> are resolved by searching the extra-env
(where they will never be found), and then the enclosing environment (where they are always bound).

|#

#|
Interestingly, even if one of the <vars> has the same symbol as u or v, there is no difference
in behavior between the two versions. In the former, the define would simply set! a new value
for the existing variable. In the latter, the variable is set! to the value, and when the variable
is looked up, it would be found in the extra environment, rather than the enclosing environment.
Thus, for a correct program -- in which defined variables can be evaluated without using any
of the (other) defined variables' values -- there would be no difference.

However, for an incorrect pgoram, e.g. <e1> : (* u 2) where u is a parameter in <vars>,
there would be a difference. In the first version, this would work as u is bound in
extended-env and has a valid valid (which is about to be overwritten). In the second version,
u is bound, but its value (prior to (set! u <e1>)) is '*unassigned*, which will signal an
error upon lookup.
|#

;; Implementation of simultaneous scope rule for internal definitions without constructing
;; the extra frame:

#|

One option is an elaborate program transformation in which the defined variables are added
to <vars>, the <args> receive additional '*unassigned*'s, and the unassigned variables are
set! using the same method as in the let version. However, addition of '*unassigned*'s
to the <args> presents a problem -- it could be done, but it would be an extraordinary mess.

An alternative is to scan out the defines (in whatever order they appear), then
re-order the body to place define statements before any other expressions.
This works for any procedure in which in internal definitions have
value expressions which do not use any of the defined variables.
This can be implemented by re-using scan-out-defines, but with the modification
that after separating out the defines and regulars, the regulars are appended to the defines,
e.g.
(append (reverse defines) (reverse regulars)) ;; assuming that we used cons in partition

Why it works: there is no difference between sequential and simultaneous programs which
do not have internal definitions that use any of the defined variables.

Examples:

<vars> : (x y)
(define u (* x 2))                This program is free from defines that use
(define v (+ y 3))                internally-defined variables


<vars> : (v y)
(define u (* v 2))                This program contains defines that use variables bound
(define v (+ y 3))                in the environment, then re-defines these variables

<vars> : (x y)
(define u (* x 2))                Contains defines that use internally-defined variables
(define v (+ u x y))

<vars> : (x y)
(define u (* x 2))                Contains defines that use internally-defined variables,
(set! u (+ u 1))                  and if re-ordered, has different value.
(define v (+ u x y))


Thus, this is not guaranteed to work when internally-defined variables use any of the
variables' values.

|#
