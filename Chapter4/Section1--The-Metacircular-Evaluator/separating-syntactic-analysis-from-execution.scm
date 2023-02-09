;; 4.1.7 Separating Syntactic Analysis from Execution

;; Ex. 4.22
#|
It should be sufficient to add the following to the definition of analyze,
prior to "application?" :
((let? exp) (analyze (let->combination exp)))
Moreover, if we have modified let->combination as in Ex. 4.8 to accommodate
named-let, then we get both. In essence, if we can handle cond->if as a derived expression
 then let->combination can be handled analogously.
|#

;; Ex. 4.23
#|
analyze-sequence in the text builds up a single call from a sequence of expressions

(<exp1> <exp2> <exp3> ... <expN>)

                 |
                 |
                 v

(<proc1> <proc2> <proc3> ... <procN>)

Then,
(lambda (env) (proc1 env) (proc2 env))
(lambda (env) ((lambda (env) (proc1 env) (proc2 env)) env) (proc env))

Each iteration of analyze-sequence produces a lambda taking an env which calls the current
and next procedures on env. The final result is a procedure of n-1 lambda's nested
in a single call.
All of this occurs at analysis time, so that during execution, only the procedure is applied.
In comparison, Alyssa's version of execution involves a loop. Thus, when called,
Alyssa's execution procedure will loop through the n procedures, performing:

1 cdr                 |
1 null?               | else branch
1 car                 |
1 cdr                 |

1 cdr                 |
1 null?               | null? branch
1 car                 |

Hence, for a expression list of length n:

   else branch              null? branch
________________          ________________
    2(n-1) cdr      +          1 cdr           = 2n-1 cdr
    n-1 car         +          1 car           = n car
    n-1 null?       +          1 null?         = n null?


Alyssa's
________
1 expression: 1 cdr, 1 car, 1 null?, 1 procedure call
2 expressions: 3 cdr, 2 car, 2 null?, 2 procedure call


Text
____

1 expression: 1 procedure call
2 expression: 2 procedure call (technically, 3 if we include the lambda call)

For n expressions, we have 1 + n-2 + n total calls, including the lambda calls,
thus, n-1 + n total calls, i.e. n-1 lambda calls + n procedure calls


Both approaches add the overhead of calling the (lambda (env) ...),
but the text version accrues more of them (for n > 2) than Alyssa's version.
The version in the text introduces n-1 lambda's overhead -- though,
since these occur in the implementation language, we expect them to be small
compared to the cost of the loop (to be fair, we'd also need to account for loop
overhead itself, beyond the cdr's, null?'s and car's).
|#

;; Ex. 4.24
#|
Iterative procedures are a suitable way to create worthy benchmarks, as the basic
operations remain simple and the sources of overhead will primarily be those of looping
overhead: loop predicate evaluation, loop consequent evaluation.
Beyond loops, deep expressions are likely to lead to very different performance.
Furthermore, large sequences of expressions will have very different performance.
|#
