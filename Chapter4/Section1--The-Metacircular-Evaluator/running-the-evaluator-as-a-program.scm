;; 4.1.4 Running the Evaluator as a Program

;; Ex. 4.14
#|
When entered into the metacircular evaluator, this adds a procedure to the
metacircular evaluator's environment. As it is defined in terms of the
metacircular evaluator's language, the special forms, derived expressions
and primitive operations are all evaluated on the implemented language.

On the other hand, Louis' installation of the map primitive into the
implemented language using the implementation language's map will result in
the implementation language operating on the representation of the procedures
and expressions belong to the implemented language. Given that our procedures
are represented as lists, the implementation language's map will choke on the
first attempt at procedure application.

We would end up with a call to apply that looks like:
(apply map args)
Where args is a list of expressions, the first of which will be taken
to be the proc and the rest either directing to the vararg version of map,
or, if only two args, then the list to which proc will be applied.
The former will assuredly fail catastrophically, and the latter will also
fail, but perhaps less catastrophically.
|#

;; Further elaboration on 4.14
#|
Given Louis' installation of map as a primitive from the implementation
language, consider:

(define (square x) (* x x))
(map square '(1 2 3))    ; <---- recognized as application

map itself will be recognized as a primitive procedure inside apply, hence,
apply-primitive-procedure will be called on the 2-element list produced by calling
eval on the 2-element list of 'square and '(1 2 3).
In the implementation language, this results in (map proc seq)
                                                      ^    ^
                                                      |    |
                                                      |   3-element list ( '(1 2 3) )
                                                 4-element list (the procedure object)

Inside the body of map in the implementation language, the first call is (null? seq),
which, because we are sharing list representation between the implementation and implemented
language, happens to not fail. Next we have (proc (car seq)). (car seq) happens to
not fail for the same reason as null?. However, when proc is called, failure is guaranteed.
In essence, in the implementation language, we are attempting to call a 4-element list
on a value. But the implementation language does not recognize lists as procedures
(not does it know how to apply them).

See p. 131 for illustrations.
|#
