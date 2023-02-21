;; 4.4.3 Is Logic Programming Mathematical Logic?

;; Ex. 4.64
#|
This yields an infinite loop as (outranked-by ?middle-manager ?boss)
is a pattern which can always be unified with the next call as follows:
(supervisor ?staff-person ?boss) now represents two patterns:
?middle-manager and ?who, thus, the frame created represents a successful
unification (with nothing bound, but with some requirements if a a later time
either variable is assigned). Hence, one proceeds to the next call of
(outranked-by ?middle-manager ?boss) which initiates the cycle anew.
|#

;; Ex. 4.65
#|
Let's break down the stream of frames for the wheel rule:

(supervisor ?middle-manager ?person) : this is satisfied by ?person = (Bitdiddle Ben)
                                       for ?middle-manager = (Hacker Alyssa P)

(supervisor ?middle-manager ?person) : this is satisfied by ?person = (Warbucks Oliver)
                                       for ?middle-manager = (Bitdiddle Ben)
                                                             (Scrooge Eben)
                                                             (Aull Dewitt)

Consider the first frame:
+----------------------------------+
| ?person: (Warbucks Oliver)       | ----> (supervisor ?x ?middle-manager)
| ?middle-manager: (Bitdiddle Ben) |   satisfied by ?x = (Hacker Alyssa P)
+----------------------------------+                     (Fect Cy D)
                                                         (Tweakit Lem E)

Thus, 3 frames result from this pathway.

Consider the second frame:
+----------------------------------+
| ?person: (Warbucks Oliver)       | ----> (supervisor ?x ?middle-manager)
| ?middle-manager: (Scrooge Eben)  |   satisfied by ?x = (Cratchet Robert)
+----------------------------------+

Thus, 1 frame results from this pathway.

Consider the third frame:
+----------------------------------+
| ?person: (Warbucks Oliver)       | ----> (supervisor ?x ?middle-manager)
| ?middle-manager: (Aull Dewitt)   |   no assignments satisfy this pattern.
+----------------------------------+

Thus, there are 4 frames produced which bind ?person as (Warbucks Oliver)
|#


;; Ex. 4.66
#|
Ben has realized that the output stream is not necessarily unique --
multiple results may match a pattern, producing a frame for each, but while
the combination of values bound to the variables during the production of each frame
may be unique, the final frame produced by the query may not be.

Possible solutions
__________________

The wrong approach
__________________
Application of a uniqueness test to the values extracted from each frame.
Might work for text, but only under very limited conditions.

The right approach
__________________
Produce a stream with some inherent ordering, perhaps in a manner analogous to
Ex. 4.60. This, however, necessitates that queries be formulated with
an additional and clause which induces the ordering -- not easy one we
allow complex queries.

Notably, Ben's strategy works for simple and compound queries the results of which
are inherently unique. e.g.
(and (job ?x (computer programmer)) (salary ?x ?amount))

Alternatively, Ben can simple declare that only queries which produce unique
output streams are valid inputs to his system.
|#


;; Ex. 4.67
#|
To detect the loop, we must find a call which either forwards a pattern without
causing it to be bound, or introduces another dummy pattern;
the call must be of the same produced which initiated this body evaluation.

In essence, we can record the procedure being called and the argument list of the
call, then, if the next procedure's body causes an evaluation of the same procedure
with the same arguments, we will have detected a simple infinite loop.

Construct a history by storing the procedure, argument values and argument variables
which lead to creation of each frame. Then, we set a size for how many frames to
keep in our history.
A queue should be used to store the collections of initializer arguments --
when we exceed the set size, we delete! a collection from the front and add the
new collectionto the end.

Prior to each next step, we initiate a scan, checking the first argument against
all the rest, the second argument against all the rest, etc.
|#

(define (loops? history)
  (define (iter (first rest))
    (cond ((null? rest) false)
          ((equal? first (car rest)) true)
          (else (iter (first (cdr rest))))))
  (let ((first (car history))
        (rest (cdr history)))
    (if (null? rest)
        false
        (let ((result (iter first rest)))
          (if result
              result
              (loops? rest)))
        ;; Equivalently, but more obscure
        ;; (or (iter first rest) (loops? rest))
        )))


;; Ex. 4.68
;; The procedural model for the rule
(define (my-reverse x)
  (define (accum init a)
    (if (null? a)
        init
        (append (accum init (cdr a))
                (list (car a)))))
  (accum '() x))

;; The reverse of '() is '()
(rule (reverse () ()))
;; This works on both cases
(rule (reverse (?u . ?v) ?x)
      (and (reverse ?v ?z)
           (append-to-form ?z (?u) ?x)))


;; Ex. 4.69
(rule (ends-in-grandson (grandson)))
(rule (ends-in-grandson (?x . ?y))
      (ends-in-grandson ?y))

;; In essence, for each great, we find the father and then proceed to next
;; having peeled one great.
(rule ((great . ?rel) ?x ?y)
      (and (ends-in-grandson ?rel)
           (son-of ?x ?f)
           (?rel ?f ?y)))
;; To match the case without any preceding greats
(rule ((grandson) ?g ?s)
      (grandson-of ?g ?s))

