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
            (list (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
                            (append (map (lambda (x y) (list 'set! x y)) vars vals)
                                    regulars))))))))


;; Version 1.2: in forward order, the short way
(define (scan-out-defines exps)
  (let ((defines (filter definition? exps))
        (regulars (filter (lambda (x) (not (definition? x))) exps)))
    (if (null? defines)
        exps ;; or, regulars, which is equivalent
        (let ((vars (map definition-variable defines))
              (vals (map definition-value defines)))
          (list (make-let (map (lambda (x) (list x ''*unassigned*)) vars)
                          (append (map (lambda (x y) (list 'set! x y)) vars vals)
                                  regulars)))))))

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
        (list (make-let bindings exps))
        (let ((var (definition-variable (car defines)))
              (val (definition-value (car defines))))
          (iter (cdr defines)
                (cons (list var ''*unassigned*) bindings)
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
            (list (make-let bindings (append set!-exps body-exps))))
        (let ((first (car exps))
              (rest (cdr exps)))
          (if (definition? first)
              (let ((var (definition-variable first))
                    (val (definition-value first)))
                (iter (append bindings (list (list var ''*unassigned*)))
                      (append set!-exps (list (list 'set! var val)))
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

However, for an incorrect program, e.g. <e1> : (* u 2) where u is a parameter in <vars>,
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



;; Ex. 4.18

;; (lambda <vars>
;;   (let ((u '*unassigned*)
;;         (v '*unassigned*))
;;     (let ((a <e1>)
;;           (b <e2>))
;;       (set! u a)
;;       (set! v b))
;;     <e3))

#|
In addition to creating the same two environments (extended-env and extra-env)
as the version in the text, the inner let creates a third environment:



                                 extra-env
                                    ^
                                    |
                                    |
                        +------------------+
                        | a: result of <e1>|
                        | b: result of <e2>|
   inner-env  --------->|                  |
                        |                  |
                        |                  |
                        +------------------+

<e1>, <e2> are evaluated in extra-env. At the time of their evaluation,
u, v are still bound to '*unassigned*, so that if either <e1> or <e2> depend
on u (or v) being bound to a value, an error will occur.

The difference is that this version enforces the restriction that define variable
values can be evaluated without using any of the variables' values by forcing the
evaluation of <e1>, <e2> to occur when u, v are still bound to '*unassigned*.

The version in thetext will also evaluate <e1> when u is bound to '*unassigned*,
but <e2> will be evaluated with u bound to the result of <e1>.

Thus, the version in the exercise is more strict; it is closer to the meaning of
"simultaneous".


solve will not work if the transformation in this exercise is used, as while
<e1> : (integral (delay dy) y0 dt)
can be evaluated with dy bound to '*unassigned*, it is not possible to evaluate
<e2> : (stream-map f y)
with y bound to '*unassigned*.

solve will work if the transformation in the text is used, as <e1> can be
evaluated as-is, and then y is set! to the result of <e1>. Then, when <e2>
is evaluated, y is not bound to '*unassigned*, hence, it will not error.

|#



;; Ex. 4.19

#|
See p. 138 for illustrative expansion of environment and syntax.

Application of the simultaneous scope rule in the text will result
in an error as a is bound to '*unassigned* at the time (+ a x) is evaluated.
While a is bound to something other than '*unassigned* in the enclosing environment,
the environment in which the define's occur really should be separate.

In essence, if we treat procedure definitions differently from variable definitions,
i.e. apply the sequential rule for variables and simultaneous rule for procedures,
then we quickly devolve into special cases. We would need to ask at every define that
looks like a variable: is the value actually a procedure?
as we could simply prefer to write our procedure as (define f (lambda (...) ...))
which look like variables (and are), but would in fact need to be treated using
the simultaneous rule, not sequential.

Thus, the only sane choices are either the simultaneous rule in the text, or a truly
simultaneous implementation.
|#

#|

Interestingly, if one reversed the order, i.e. (define a 5) (define b (+ a x)),
no error would occur using the definition of simultaneous scope in the text --
but an error would still result from the definition of simultaneous scope in Ex. 4.18.

To implement internal definitions as Eva prefers requires that one re-order define's
in such a way that each define will have assigned variables in its expression. Then,
the define's could be executed sequentially. However, as we saw in Ex. 4.18, this does not
guarantee simultaneity in the strict sense, which requires that each variable-being-defined
be independent of the other variables-being-defined in that scope.

If we insist upon independent simultaneity, then Ex. 4.18 gives us that.

If we pursue Eva's conditional simultaneity, then we encounter thorny questions about
valid sequential orderings which appear to be simultaneous, but are in fact partially sequential.

For example, what would one do about:
(define b (+ a x))
(define a 5)
(define a 3)

Which a comes first? and why could we not permit such a re-ordering as:
(define a 3)
(define b (+ a x))
(define a 5)
;; do something with a

One could suggest that the re-ordering follow the sequential definition of identical variables
in the source code, rearranging only the variables that are not identical into some order
which can be evaluated without encountering an unassigned variable -- identical variables
could be rearranged as blocks, but then what order in the block? (sequential as per the source?)

Very quickly, however, it becomes difficult to reason about what a block of code will actually
do, as the only general mechanism we can specify is to find a feasible ordering for evaluation.
However, there will always be multiple feasible orderings once we move beyond two variables.

Thus, it begins to become clear why the MIT implementers of Scheme prefer the definition
of simultaneity in the text -- it is unambiguous, easy to reason about, and efficient to
implement, while also permissive around sequential definitions in simultaneous scope,
which, as shown in Ex. 4.18, can be a desirable property.


--Another demonstration of the difficulty of true simultaneity

In fact, even with only two variables, one can obtain multiple feasible orderings, e.g.
(define a 3)
(define b (+ a x))
(define a 5)
(define b (- a x))

If we forbid multiple defines on the same variable, then perhaps we could determine
appropriate orderings for
(define c (* b x))
(define b (+ a x))
(define a 5)

However, this would require a full analysis of each expression, followed by
a search of feasible orderings (of which there are always multiple);
this is only applicable to "simple" definitions. Something such as
(define c (begin (set! a (+ a 1)) (* b x)))
wreaks havoc upon such a strategy -- and this is explicit mutation!
We have to expect and accommodate the possibility of interior mutation (i.e. on data structures)
thus, we would have to analyze the entirety of each expression to determine whether
it mutates (and whether said mutation would have no effect...).
Moreover, the behavior of code may be quite unexpected compared to hat is written,
as the rules would be highly convoluted.
One hopeful language designer might suggest: no mutation!
But do we really wish for such a cage, all for the sake of "true" simultaneity?
(the benefits of which appear quite doubtful indeed).

|#



;; Ex. 4.20

#|

 (letrec ((<var1> <exp1>) ... (<varN> <expN>))
   <body>)

Transformation to form in text

 (let ((<var1> '*unassigned*) ... (<varN> '*unassigned*))
   (set! <var1> <exp1>)
   .
   .
   .
   (set! <varN> <expN>)
   <body>)


Transformation to form in Ex. 4.18

 (let ((<var1> '*unassigned*) ... (<varN> '*unassigned*))
   (let ((<hvar1> <exp1>) ... (<hvarN> <expN>))
     (set! <var1> <hvar1>)
     .
     .
     .
     (set! <varN> <hvarN>))
   <body>)

|#

;; a

(define (letrec? exp) (tagged-list? exp 'letrec))
(define (letrec-bindings exp) (cadr exp))
(define (letrec-body exp) (cddr exp))

(define (letrec-variables exp)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (caar bindings)
              (iter (cdr bindings)))))
  (iter (letrec-bindings exp)))
;; Or, more tersely:
(define (letrec-variables exp) (map car (letrec-bindings exp)))

(define (letrec-exps exp)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (cadar bindings)
              (iter (cdr bindings)))))
  (iter (letrec-bindings exp)))
;; Or, more tersely:
(define (letrec-exps exp) (map cadr (letrec-bindings exp)))

;; Syntax transformation to form in text
(define (letrec->let exp)
  (let ((vars (letrec-variables exp)))
    (make-let (map (lambda (var) (list var ''*unassigned*)) vars)
              (append (map (lambda (var exp) (list 'set! var exp)) vars (letrec-exps exp))
                      (letrec-body exp)))))

;; Syntax transformation to form in Ex. 4.18
(define (letrec->let exp)
  (let ((vars (letrec-variables exp)))
    (let ((hvars (map (lambda (var) (join-symbol 'h var)) vars)))
      (make-let (map (lambda (var) (list var ''*unassigned*)) vars)
                (cons (make-let (map (lambda (hvar exp) (list hvar exp)) hvars (letrec-exps exp))
                                (map (lambda (var hvar) (list 'set! var hvar)) vars hvars))
                      (letrec-body exp))))))

;; within eval:
((letrec? exp)
 (eval (letrec->let exp) env))



;; b

#|
We can assume that we implement simultaneous scope as in the text. This yields:

 (define (f x)
   (letrec ((even? (lambda ...))
            (odd? (lambda ...)))
     <rest-of-body-of-f>))

Which transforms to:

 (define (f x)
   (let ((even? '*unassigned*)
         (odd? '*unassigned*))
     (set! even? (lambda ...))
     (set! odd? (lambda ...))
     <rest-of-body-of-f>))

Which, after we expand the let, transforms to:

 (define (f x)
   ((lambda (even? odd?)
      (set! even? (lambda ...))
      (set! odd? (lambda ...))
      <rest-of-body-of-f>)
    '*unassigned*
    '*unassigned*))


Calling f on 5 creates the environment below.


                                    ^
                                    |
                                    |
                        +------------------+
                        | x : 5            |
                        |                  |
    env-of-f  --------->|                  |
                        |                  |
                        |                  |
                        +------------------+


Then, we generate the (lambda) procedure and call it on the '*unassigned*s,
creating this environment:

                                 env-of-f
                                    ^
                                    |
                                    |
                        +-----------------------+
                        | even? : '*unassigned* |
                        | odd? : '*unassigned*  |
    extended-env ------>|                       |
                        |                       |
                        |                       |
                        +-----------------------+


We proceed to evaluate the body of the procedure within extended-env.
The first expression creates a procedure, the environment part of which is extended-env,
and binds it to even?.
The second expression creates a procedure, the environment part of which is extended-env,
and binds it to odd?.
Then, the rest of the body of f is evaluated.

Thus, when even? or odd? is called within <rest-of-body-of-f>, the respective internal
call to odd? or even? can be resolved as their environment part is extended-env,
which contains procedures bound to the variables.

                                 extended-env
                                    ^
                                    |
                                    |
                        +------------------+
                        | n : some value   |
                        |                  |
 created by even? ----->|                  |
        or odd?         |                  |
                        |                  |
                        +------------------+

|#

#|

Louis' suggestion corresponds to

 (define (f x)
   (let ((even? (lambda ...))
         (odd? (lambda ...)))
     <rest-of-body-of-f>))

Which, after we expand the let, transforms to:

 (define (f x)
   ((lambda (even? odd?) <rest-of-body-of-f>)
    (lambda ...)
    (lambda ...)))


The (lambda ...)'s for even?, odd? are evaluated in the environment of f, hence,
they have as their environment part the env-of-f.


The evaluation of the lambda and its call on the two procedure objects
produces extended-env.

                                 env-of-f
                                    ^
                                    |
                                    |
                        +-----------------------+
                        | even? : proc          |
                        | odd? : proc           |
    extended-env ------>|                       |
                        |                       |
                        |                       |
                        +-----------------------+


Then, the <rest-of-body-of-f> is evaluated.
When even? or odd? is called with n != 0, the variable odd? or even? will need
to be looked up. However, the environment part of the procedure will be env-of-f,
hence:

                                 env-of-f
                                    ^
                                    |
                                    |
                        +------------------+
                        | n : some value   |
                        |                  |
 created by even? ----->|                  |
        or odd?         |                  |
                        |                  |
                        +------------------+

The variable lookup will fail in this case, as even? and odd? are not bound
in the environment of f.

On the other hand, with letrec, the call environment is extended-env, and the enclosing
environment of the frame created by calling even? will be extended-env, and extended-env
has even? and odd? bound (to the correct procedures).

|#


;; Ex. 4.21

;; a

((lambda (n)
   ((lambda (fib)    ;; the initiator of the recursion using the lambda passed as argument.
      (fib fib n))   ;; By passing the procedure to the procedure, the procedure can then
    (lambda (fb k)   ;; call itself at will. Otherwise, recursion requires a named variable.
           (cond ((= k 0) 0)
                 ((= k 1) 1)
                 (else (+ (fb fb (- k 1))
                          (fb fb (- k 2))))))))
 10)


;; b

;; In essence, pass the procedures in the same positions as the initiator.
;; Thus, the internals of each procedure receive the procedures in the
;; same positions so that no confusion can arise about which procedure is bound
;; to which variable.

(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0)
         true
         (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0)
         false
         (ev? ev? od? (- n 1))))))
