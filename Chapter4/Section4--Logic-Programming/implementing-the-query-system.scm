;; 4.4.4 Implementing the Query System

;;;; 4.4.4.5 Maintaining the Data Base

;; Ex. 4.70
#|
As the stream-cdr is delayed,
(set! THE-ASSERTIONS (cons-stream assertion THE-ASSERTIONS))
results in a circular definition in which the car is assertion the cdr
points back to the pair.

If the stream-cdr were not delayed, then this would work as (Louis?) intended,
as THE-ASSERTIONS would be looked up before the set!

The purpose of the let bindings is to look up and store the current value
bound to the symbol THE-ASSERTIONS; this value can then be placed in the
stream-cdr of the cons-stream, and the set! proceed as intended.
|#


;; Ex. 4.71
#|
Delayed rule application can be desirable from a number of perspectives --
at simplest, if one uses diabolical rules, then at least one has a hope of
executing some portion of a query involving an infinite recursion.

It is helpful consider the computational complexity, in particular, the space
requirements. Delaying apply-rules reduces the amount of space required
at any given point in the query evaluation, as find-assertions provides a ready
stream of frames from which subsequent computations can continue -- thus,
we need only force the delayed list when we run out of available elements.
In the context of a query language, simple queries appear ubiquitously
(they are the atomics of the language), thus, by delaying the apply-rules
of each, one reduces the actively occupied space by 1/2 for a single simple query,
suppose that find-assertions and apply-rules produce streams of the same size.
Apply-rules will typically produce a stream much larger than find-assertions,
thus, an equal split represents a very conservative estimate.

Given this analysis of a single stream, we must remember that real queries
will typically be comprised of multiple simple queries in series (and) or parallel (or).
Let us consider the series cases first, then discuss the parallel case with disjoin.

Consider a series of simple queries which occur in an and:
(and <query1> <query2> <query3> ...)

                   find-assertions
                 +--------------------> s_1_1 ----+                                 +----> s_2_1
+----------+     |                                |                +----------+     |
| <query1> | ----+                                +----> S_1 ----> | <query1> | ----+
+----------+     |                                |                +----------+     |
                 +--------------------> s_1_2 ----+                                 +----> s_2_2
                   apply-rules



Let us suppose that (length s_1_1) = 1, (length s_1_2) = 1, thus, (length S_1) = 2
when s_1_2 is not delayed.

s_2_1 is therefore going to pattern-match against 2 frames. Let us assume that a single
match is found for each, yielding 2 extended frames.

s_2_2 is therefore going to pattern-match against 2 frames. Let us assume that a single
unification is found for each, yielding 2 extended frames.

Thus, we have 2 extended frames, given only the assumption that a single match + single unification
is possible for each frame. When these are propagated into the second simple query,
if we against assume single match + single unification for each frame, then we have
4 extended frames. If propagated to the third simple query under the same assumption,
then we have 8 extended frames.

This is exponential growth in space, 𝒪(2^n) where n is the number of simple queries
in series, and the 2 arises from the assumption of single match + single unification.


If instead we delay the rule application, then at each simple query we would produce
a single frame (under the assumption of single match + single delayed unification),
leading to, at worse, 𝒪(n) space, assuming that we store all extended frames
simultaneously. If we permit the same assumption as was implicit in the non-delayed
estimate of space, which was that each point in the sequence of queries we store only
the frames extended by find-assertions and apply-rules to the input stream,
then we our space complexity is only 𝒪(1). (Perhaps, more realistically, we have 𝒪(n)
as the delayed unification is not free, and we do accrue a chain of n of them).

Thus, by delaying apply-rules, we extend only the number of frames which is
sufficient to proceed with the computation at any point during execution,
conferring 𝒪(1) complexity in space as opposed to 𝒪(2^n).

Compound queries will be comprised of quite a few simple queries, and rules
are comprised of both simple and compound queries, thus, we might easily expect to
encounter n >= 10 with toy queries and perhaps up to n = 50 for "real" queries.
2^n    :    n = 10    =>    KiB
            n = 20    =>    MiB
            n = 30    =>    GiB
            n = 40    =>    TiB
            n = 50    =>    PiB

Thus, even if extended frame costs 1 byte after n =30, we have effectively
no hope of completing a query. Quite likely, an extended frame will cost
between (2^5, 2^10), hence, we are actually far more restricted.

An example for simple-query?
(and (job ?x ?y) (address ?x ?z) (supervisor ?x ?a) (?b ?x ?c))



The case with disjoin is a little less explosive than simple-query.
With delay, disjoin creates a single stream of extended frames at a time,
whereas without delay it creates all streams and then proceeds with the merge.
Hence, 𝒪(1) space to 𝒪(n) space, assuming an or-statement with n queries.
|#
