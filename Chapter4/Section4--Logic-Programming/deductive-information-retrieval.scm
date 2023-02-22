;; 4.4.1 Deductive Information Retrieval

;; Ex. 4.55

;; a
(supervisor ?x (Bitdiddle Ben))

;; b
(job ?x (accounting . ?y))

;; c
(address ?x (Slumerville ?y ?z))
;; or, more flexibly:
(address ?x (Slumerville . ?y))


;; Ex. 4.56

;; a
(and (supervisor ?x (Bitdiddle Ben))
     (address ?x ?y))

;; b
(and (salary (Bitdiddle Ben) ?amount)
     (salary ?person ?other-amount)
     (lisp-value < ?other-amount ?amount))

;; c
(and (supervisor ?person ?super)
     (not (job ?super (computer . ?type)))
     (job ?super ?division))


;; Ex. 4.57

;; Version 1: the long way. Also, the early not's likely break the query.
(rule (can-replace ?person-1 ?person-2)
      (or (and (not (same ?person-1 ?person-2))
               (job ?person-1 ?job-1)
               (job ?person-2 ?job-2)
               (same ?job-1 ?job-2))
          (and (not (same ?person-1 ?person-2))
               (job ?person-1 ?job-1)
               (job ?person-2 ?job-2)
               (can-do-job ?job-1 ?job-2))))

;; Version 2: more direct expression of the rule
(rule (can-replace ?person-1 ?person-2)
      (and (job ?person-1 ?job-1)
           (job ?person-2 ?job-2)
           (not (same ?person-1 ?person-2))
           (or (same ?job-1 ?job-2)
               (can-do-job ?job-1 ?job-2))))


;; a
(can-replace ?person (Fect Cy D))

;; b
(and (can-replace ?person-1 ?person-2)
     (salary ?person-1 ?amount-1)
     (salary ?person-2 ?amount-2)
     (lisp-value > ?amount-2 ?amount-1))


;; Ex. 4.58

(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?type-1))
           (supervisor ?person ?super)
           (job ?super (?super-division . ?type-2))
           (not (same ?division ?super-division))))


;; Ex. 4.59

;; a
(meeting ?type (Friday ?time))

;; b
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?division . ?type))
               (meeting ?division ?day-and-time))))

;; c
(meeting-time (Hacker Alyssa P) (Wednesday ?time))


;; Ex. 4.60
#|
This occurs because both results match the pattern.
At the fundamental level, the truth value is the same irrespective of how we permute
the pattern; only by selecting 1 of the two referents can we even create a one-sided
relationship which could be used to eliminate permutations.
Even if we iterated through the fixed referents, the results would still need to be
filtered for permutations. This would necessitate a mechanism in the query language
itself, which would filter to combination rather than permutations.
I do not think that this can be expressed within the query language other than
through a special form, as to eliminate non-unique combinations requires that
one have in hand the set of all permutations.
|#

;; This is satisfied by both permutations of ?person-1 and ?person-2,
;; thus, we have the same problem as in the text.
(rule (lives-near-combination ?person-1 ?person-2)
      (and (lives-near ?person-1 ?person-2)
           (lives-near ?person-2 ?person-1)))

;; It may be possible to break this cycle by imposing a third conditional
;; statement which confers a total ordering on all permutations of ?person-1 and ?person-2.
;; An ordering would confer uniqueness; we might consider lexicographic order
;; here since we are dealing with person-names.

(rule (lives-near-combination ?person-1 ?person-2)
      (and (lives-near ?person-1 ?person-2)
           (lives-near ?person-2 ?person-1)
           (lisp-value lexicographic<? ?person-1 ?person-2)))

;; since we are dealing with symbols (but we could just use symbol<?).
;; more generally, we could define lexicographic<? to handle the various types
;; of inputs we might expect
(define (lexicographic<? a b)
  (cond ((and (symbol? a) (symbol? b))
         (symbol<? a b))
        ((and (string? a) (string? b))
         (string<? a b))
        ((and (number? a) (number? b))
         (< a b))
        ((and (pair? a) (pair? b))
         ;; (fold-left (lambda (x y) (and x y)) true (map lexicographic<? a b))
         (lexicographic<? (car a) (car b)))
        (else
         (error "Unexpected types -- lexicographic<?" a b))))


;; Ex. 4.61
;; Query input:
(?x next-to ?y in (1 (2 3) 4))
;; Query output:
(1 next-to (2 3) in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))

;; Query input:
(?x next-to 1 in (2 1 3 1))
;; Query output:
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))


;; Ex. 4.62
(rule (last-pair (?x) (?x)))
(rule (last-pair (?x . ?y) ?z)
      (last-pair ?y ?z))

#|
The rules seem to work on all 4 queries.

Take 1:
If we had used (?z) to assert that the second rule deals only with single-element
lists, then the 4th query would technically match and infinite recursion could be
initiated as any list can have a last pair of (3).
Instead, by allowing the single-element list to match only the final check,
the only pattern that can match is the first rule.

Revised thought:
Actually, the answer depends on the order in which rules are applied. If the first
rule is applied first, then we have a successful termination. When the second rule
is applied, we have an infinite recursion.

Thus, we have a problem with queries such as (last-pair ?x (3)) as there are an
infinite number of ?x's which satisfy this pattern.
|#


;; Ex. 4.63
(rule (grandson ?g ?s)
      (and (son ?g ?f)
           (son ?f ?s)))
(rule (son-by-wife ?m ?s)
      (and (wife ?m ?w)
           (son ?w ?s)))

;; grandson of Cain
(grandson Cain ?s)

;; sons of Lamech
(son-by-wife Lamech ?s)

;; grandsons of Methushael
(and (son Methushael ?s)
     (son-by-wife ?s ?g))

;; Incorporates both ways to identify a son
(rule (son-of ?m ?s)
      (or (son ?m ?s)
          (son-by-wife ?m ?s)))

;; Uses more robust son-finding
(rule (grandson-of ?g ?s)
      (and (son-of ?g ?f)
           (son-of ?f ?s)))
;; which enables us to re-write the third query
(grandson-of Methushael ?s)
