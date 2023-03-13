;; 5.5.1 Structure of the Compiler

;; Ex. 5.31
#|

(f 'x 'y)

env save/restore around evaluation of operator             : superfluous
env save/restore around evaluation of first operand        : superfluous
argl save/restore around evaluation of first operand       : superfluous
argl save/restore around evaluation of second operand      : superfluous
proc save/restore around evaluation of operand sequence    : superfluous


((f) 'x 'y)

env save/restore around evaluation of operator             : required
env save/restore around evaluation of first operand        : superfluous
argl save/restore around evaluation of first operand       : superfluous
argl save/restore around evaluation of second operand      : superfluous
proc save/restore around evaluation of operand sequence    : superfluous


(f (g 'x) y)

env save/restore around evaluation of operator             : superfluous
env save/restore around evaluation of first operand        : required
argl save/restore around evaluation of first operand       : required
argl save/restore around evaluation of second operand      : superfluous
proc save/restore around evaluation of operand sequence    : required

env save/store around first operand required because y must be
looked up in the original environment.


(f (g 'x) 'y)

env save/restore around evaluation of operator             : superfluous
env save/restore around evaluation of first operand        : superfluous
argl save/restore around evaluation of first operand       : required
argl save/restore around evaluation of second operand      : superfluous
proc save/restore around evaluation of operand sequence    : required

env save/store around first operand superfluous because the second operand
is quoted ('y), hence, there is nothing to look up in the original environment.

|#



;; Ex. 5.32

;; a
(define (application-with-symbol-operator? exp)
  (if (pair? exp)
      (if (symbol? (car exp))
          true
          false)
      false))

;; insert into eval-dispatch prior to application:
(test (op application-with-symbol-operator? (reg exp)))
(branch (label ev-application-with-symbol-operator))

;; then, have this entry point available:
(define ev-application-with-symbol-operator
  '(ev-application-with-symbol-operator
    (save continue)
    (assign unev (op operands) (reg exp))
    (assign exp (op operator) (reg exp))
    (assign proc (op lookup-variable-value) (reg exp) (reg env))
    (assign argl (op empty-arglist))
    (test (op no-operands?) (reg unev))
    (branch (label apply-dispatch))
    (save proc)
    (goto (label ev-appl-operand-loop))))


;; b
#|
This would be an extraordinary burden, in addition to being quite complex.
Furthermore, addition of new optimizations would need to consider the possibility of
interactions with all existing optimizations and the regular controller.
Also, an optimization would need to be applied at each relevant point in the controller --
this implies considerable work to ensure an optimization is applied everywhere it can be,
while respecting the existing behavior of the controller and other optimizations.
This would be the height of inflexibility, as one would need to understand the interactions
of each possible optimization, write the specific code for each, and check that all
insertions are sound -- a truly daunting task!

Moreover, this means that we do not benefit from the register need/modify analysis
enacting by the preserving mechanism. Thus, we must manually work out which save/restore
can be eliminated -- this can be quite complex once we have several branches of logic
leading to different un-/optimized paths (not all of which will have the same
need/modify behavior).

Lastly, even if we can flawlessly enact the above, the evaluator would still be interpreting
the source program, checking the type of expression, dispatching, checking operand lists, etc.
-- for which there would inevitably be overhead.

We cannot hope to be exhaustive in our use of special cases, for if we take the logic to its
conclusion, we find that every single expression is a special case which must be detected.
It is impossible to enumerate every possible expression due to the combinatorial nature of
language -- we can always add one more nested expression, thereby generating another branch
of cases to be analyzed.

A compiler (with appropriate abstractions for generic optimizations) enables us to
programmatically generate the instructions for each expression -- each special case --
without requiring us to do so manually, as the interpreter-with-optimizations would
need in order to match even a simple compiler.
|#
