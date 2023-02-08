;; 4.2.1 Normal Order and Applicative Order

;; Ex. 4.25

#|
In applicative-order Scheme, the strict evaluation of arguments requires that all 3
(predicate, usual-value, exceptional-value) be evaluated before being passed to unless.
Thus, even a call of (factorial 1) will not terminate, as the expression
(* n (factorial (- n 1))) will be evaluated, thereby initiating another evaluation
of the same expression, etc. This would work in a normal-order language as the predicate
will evaluated before the other expressions, which are only then conditionally evaluated.
|#

;; Ex. 4.26

#|

If unless is implemented as a special form, then it's merely a trivial variation
on the "if" special form.


 (unless <condition>
   <usual-value>
   <exceptional-value>)

Transforms to:

 (if <condition>
     <exceptional-value>
     <usual-value>)

|#

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp) (cadddr exp))
(define (make-unless condition usual-value exceptional-value)
  (list 'unless condition usual-value exceptional-value))
(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))

;; within eval:
((unless? exp) (eval (unless->if exp) env))
;; or, within analyze:
((unless? exp) (analyze (unless->if exp)))

#|

If unless is a procedure, then it can be passed as an argument to higher-order procedures.
A pathetic example:

(define (f proc a b c) (proc a b c))
(f unless 'x 'y 'z)    ;; z
(f list 'x 'y 'z)      ;; (x y z)

|#


;; Ex. 4.27

#|
                                 global-env
                                    ^
                                    |
                                    |
                        +------------------+
                        | x:delayed (id 10)|
                        |                  |
 created by operator -->|                  |
  procedure call        |                  |
                        |                  |
                        +------------------+

When eval-sequence is called on this procedure, eval is called on each expression
in the body of id.
(set! count (+ count 1))    is recognized as an assignment, which results in eval being
                            called on (+ count 1)
(+ count 1)                 is recognized as an application, hence, this results in
                            an apply call with a primitive procedure, which returns 1.
Thus, count is set to 1.
x                           x is recognized as a variable, hence, is looked up in the
                            enclosing environment. x is, however, a thunk.

Thus, after (define w (id (id 10))), count is 1.
Entering w in the interpreter causes w to be forced, which causes the thunk
to be forced, thereby incrementing count to 2.

On the other hand, prior to entering w in the interpreter and pressing return,
count is still 1 as w is yet to be forced. Another way to force w would be to enter
(+ w 1), after which, count would be 2.

|#

;; Ex. 4.28

#|

The operator must be forced so that apply can recognize the procedure as a valid
procedure and dispatch accordingly.

((if <predicate> + (lambda (a b) (+ a b))) 1 2)

The operator is an if-expression that evaluates to a procedure -- either primitive
or compound depending on the predicate.
One might have a rather detailed expression in the operator position, and, ultimately,
to determine how to apply, one must force the operator. One might suggest that we
can avoid forcing the evaluation of the operator by looking elsewhere first --
perhaps the arguments -- but ultimately, one will reach a state of paralysis which can
only be alleviated by an apply cycle, which necessitates that the operator be
identified (as primitive or compound).

|#


;; Ex 4.29

;; When memoized as a thunk, this otherwise long-running loop will be faster.
;; A brief aside: yes, yes, one should just use (/ (* n (+ n 1)) 2)
;; but this was the first long-running loop that came to mind.
(define (sum-from-one-to n)
  (define (iter sum i)
    (if (> i n)
        sum
        (iter (+ sum i) (+ i 1))))
  (iter 0 1))


#|

(define count 0)
(define (id x)
(set! count (+ count 1))
x)
(define (square x) (* x x))


                                 global-env
                                    ^
                                    |
                                    |
                        +-----------------------+
                        | x : delayed (id 10)   |
                        |                       |
  created by square --->|                       |
     call               |                       |
                        |                       |
                        +-----------------------+

(id 10) is not viewed in isolation, but as the operand of square. Thus,
square is the compound procedure on which apply is called.

(* x x)                is recognized as an application, but with a primitive rather than
                       compound procedure. Thus, each term in the argument list (x x)
                       is evaluated.

Without memoization, count will be incremented twice.
(once per evaluation of x).

With memoization, count will be increment once. After the first call,
the id-body-expression will be replaced by the value 10 and the environment part
of the thunk will be set to nil.

In both cases, the response of square should be 100.

|#


;; Ex. 4.30

;; a

#|
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

(for-each (lambda (x) (newline) (display x))              recognized as an application,
          (list 57 531 88))                               with for-each as compound operator.

The lambda and the sequence are both delayed, such that

                                 global-env
                                    ^
                                    |
                                    |
                        +----------------------------+
                        | proc : delayed lambda      |
                        | items : delayed (57 321 88)|
 created by for-each--->|                            |
      call              |                            |
                        |                            |
                        +----------------------------+

 (if (null? items) ;; forces the evaluation of the delayed list
     'done
     (begin (proc (car items))) ;; forced evaluation of delayed proc
     )

|#

;; b

(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

#|
                                 global-env
                                    ^
                                    |
                                    |
                        +------------------+
                        | x : delayed 1    |
                        |                  |
 created by p1 call --->|                  |
                        |                  |
                        |                  |
                        +------------------+

(set! x (cons x '(2)))                recognized as assignment, resulting in
                                      eval of (cons x '(2))

(cons x '(2))                         recognized as an application with a primitive
                                      procedure, which causes x to be looked up and '(2)
                                      to be evaluated. The result is (1 2), which is assigned
                                      to x.

For (p1 1), the result should be the same under both the original and Cy's proposal.




                                 global-env
                                    ^
                                    |
                                    |
                        +------------------------+
                        | x : delayed 1          |
                        | p : delayed definition |
 created by p2 call --->|                        |
                        |                        |
                        |                        |
                        +------------------------+

(define (p e)                recognized as definition, resulting in eval
    e                        and installation into environment (created by p2)
    x)

(p (set! x (cons x '(2))))                recognized as application with compound
                                          procedure, hence, a delayed argument


                                 env created by p2
                                    ^
                                    |
                                    |
                        +------------------+
                        | x : delayed arg  |
                        |                  |
 created by p call  --->|                  |
                        |                  |
                        |                  |
                        +------------------+

e                recognized as variable and looked up. If under original
                 original eval-sequence, the thunk remains un-forced.
                 Hence, x is actually a delayed 1, and the response is 1

If under Cy's proposal, then e will be looked up and then the thunk will be forced.
Thus, the response would be (1 2).

|#

;; c

#|

The behavior of the example in part a remains un-changed, perhaps for a few reasons.
A cheap explanation is that null? is installed as a primitive.
Likewise, "if" will trigger an actual-value call, even if we were to replace null?
with (define (compound-null? x) (null? x)).
Then we also have the use of the primitives car and cdr, which would cause force-it to
be called.
As demonstrated in part b, if we avoid bodies which explicitly call primitives,
then the behavior will differ.

|#

;; d

#|

There are potential arguments for both sides. The approach in the text has the advantage
of heavily discouraging the use of expressions for side effects alone, which (tends to)
eliminates spooky action from a distance. As mutation/assignment mix poorly with
lazy evaluation to begin with, further sidelining of mutation seems little loss --
in other words, it is confusing even in very simple programs (e.g. Ex. 4.27, 4.29),
from which we conclude that for complex programs it would be an absolute nightmare
to reason about, hence, hardly a "feature" that sane programmers would utilize.
Disallowing Cy's syntax at least makes things a little easier to reason about --
the side effect occurs only once; though, when it occurs can be quite tedious to determine.
Cy's proposal does not offer us any real relief, as memoized thunks will contain
only the result computed on the values that were bound at the time the thunk was
evaluated. Hence, the repetition of side effects will result in changing
state of whatever variables are touched, whereas each evaluated thunk is unchanging.
Thus, Cy's proposal seems even more difficult to reason about.

|#
