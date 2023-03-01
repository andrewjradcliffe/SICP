;; 4.1.5 Data as Programs

;; Ex. 4.15
#|
See p. 132 for a meandering version which leads to:

If halts? could recognize non-halting procedures, the alternative
branch will be taken and try itself would halt. But this violates the notion of halting
-- we stated that for any (p a) we can determine whether the process halts,
but in recognizing a non-halting procedure and avoiding the infinite loop
induced by (p p), we have made try halt.
How can this be? If we now say that it does indeed halt, we enter the consequent
branch -- an infinite loop of run-forever. Yet again, we have evaluated one
statement only to produce a reality which is the opposite.
Because we are able to write such a function as try which violates our
intended behavior of halts, it is clear that we cannot determine whether
any procedure halts on a given input while being consistent with the stated notion
of halting.

In essence, we use reductio ad absurdum to demonstrate that even if we assume the
existence of halts?, it is trivial to write a program which produces a contradiction.
|#
