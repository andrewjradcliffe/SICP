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

;;;; 4.4.4.8 Frames and Bindings

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
Furthermore, memory locality will be an abomination, thus, we suffer performance
loss due to low probability of the working set being in the CPU(s)'s cache(s).

An example for simple-query?
(and (job ?x ?y) (address ?x ?z) (supervisor ?x ?a) (?b ?x ?c))



The case with disjoin is a little less explosive than simple-query.
With delay, disjoin creates a single stream of extended frames at a time,
whereas without delay it creates all streams and then proceeds with the merge.
Hence, 𝒪(1) space to 𝒪(n) space, assuming an or-statement with n queries.
|#

;; Ex. 4.72
#|
In essence, if we have 𝒪(n) lists of 𝒪(m) elements each, then a literal
append requires 𝒪(nm) space. With delayed evaluation, we do not incur such a cost,
but then we return to the original motivation for interleave:
given infinite (or, simply, larger than available memory) streams,
stream-append-delayed would "get stuck" on the infinite stream and never sample
elements from any other streams.
By interleaving, we access each stream in equal amounts -- if we interleave
multiple streams recursively, e.g. (interleave s1 (interleave s2 (interleave s3 s4))),
then, at any point in the output stream, we have accessed 1/2 of s1, 1/4 of s2,
1/8 of s3 and 1/8 of s4 (assuming that s1, s2, s3, s4 are infinite).

For stream-flatmap, which is used throughout the query interpreter,
interleave enables the possibility of early failure in compound queries
(thereby reducing the number of intermediate frames created, depending on context).

For disjoin, interleave enables the possibility of early success, as success
on a later query obviates the need for success on earlier (or any other) query.
(or <query1>
    <query2>                <---- Success here removes need to proceed with any other
    <query3>
       .
       .
       .    )


Contrived examples:
(and (job ?x ?y)
     (address ?x ?z)
     (salary ?x ?w)
     (hello world goodbye))

From the point in the recursive interleave produced by conjoin (via stream-flatmap
within simple-query) at which (hello world goodbye) fails to match,
all subsequent frames will fail to match.
|#


;; Ex. 4.73
#|
flatten-stream uses delay to prevent the recursive call to flatten-stream
from being evaluated, as it would otherwise be since interleave is not a special
form that introduces delay. If not explicitly delayed, flatten-stream would
force the evaluation of each element in the original input stream, stopping
only once it reached the-empty-stream. Hence, stream-flatmap would force all
𝒪(n) of the lists generated by proc (well, force the cons-streams respective to each
to generated, hence, forcing at least the first element of each list).
Thus, we incur 𝒪(n) space complexity whereas we could have 𝒪(1) complexity -- wasteful,
potentially prohibitive when memory is limited (since it occurs throughout the system),
and likely has a performance cost due to poor memory locality.
|#


;; Ex. 4.74

;; a
(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter (lambda (x) (not (stream-null? x))) stream)))

;; b
#|
As there is only 1 possible interleaving order for a list comprised of elements
which are either the empty stream or a singleton stream, there should not
be any change in query system behavior.
|#


;; Ex. 4.75

(define (singleton-stream? stream)
  (cond ((stream-null? stream) false)
        ((stream-null? (stream-cdr stream)) true)
        (else false)))

(define (unique-query exps) (car exps))
(define (uniquely-asserted operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (let ((new-frames (qeval (unique-query operands)
                              (singleton-stream frame))))
       (if (singleton-stream? new-frames)
           new-frames
           the-empty-stream)))
   frame-stream))

;; query to list all people who supervise one person
(and (supervisor ?x ?y)
     (unique (supervisor ?z ?y)))


;; Ex. 4.76

(define (conjoin-improved conjuncts frame-stream)
  (cond ((empty-conjunction? conjuncts)
         frame-stream)
        ((empty-conjunction? (rest-conjuncts conjuncts))
         (qeval (first-conjunct conjuncts) frame-stream))
        (else
         (let ((first (first-conjunct conjuncts))
               (second (first-conjunct (rest-conjuncts conjuncts)))
               (rest (rest-conjuncts (rest-conjuncts conjuncts))))
           (conjoin-improved rest (stream-merge-if-compatible
                                   (qeval first frame-stream)
                                   (qeval second frame-stream)))))))

(define (stream-merge-if-compatible frame-stream1 frame-stream2)
  (stream-flatmap
   (lambda (frame2)
     (stream-flatmap
      (lambda (frame1)
        (merge-if-compatible frame1 frame2))
      frame-stream1))
   frame-stream2))

(define (merge-if-compatible frame1 frame2)
  (cond ((null? frame1) frame2)
        ((null? frame2) frame1)
        (else
         (let ((b (car frame1)))
           (let ((extended-frame
                  (extend-if-possible (binding-variable b) (binding-value b) frame2)))
             (if (eq? extended-frame 'failed)
                 the-empty-stream
                 (merge-if-compatible (cdr frame1) extended-frame)))))))

#|
The question is: do we need to re-name variables?
I think not, as these are appearing as part of an and, hence, the repetition of
variable names is in fact necessary.
|#
