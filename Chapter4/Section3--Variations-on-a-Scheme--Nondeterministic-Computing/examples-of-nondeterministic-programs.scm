;; 4.3.2 Examples of Nondeterministic Programs

;; Ex. 4.38
#|
simply remove: (require (not (= (abs (- smith fletcher)) 1)))

i.e.
|#

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))


#|
The modified puzzle admits 5 solutions:

Initial
_______
smith
cooper
baker
fletcher
miller

Feasible#1
__________
smith
fletcher
baker
cooper
miller

Feasible#2
__________
baker
fletcher
smith
cooper
miller

Feasible#3
__________
baker
cooper
smith
fletcher
miller

Feasible#4
__________
baker
cooper
miller
fletcher
smith

|#


;; Ex. 4.39

#|
If we consider the restrictions checked prior to rejection, then it does
seem that some orderings would be faster than others, as some requirements
are more easily satisfied than others. Namely, the requirements involving more than
1 entity are likely to have fewer feasible solutions due to simple permutation.
In essence, one can reject a proposed solution by examination of any given
constraint, but it is possible to satisfy certain constraints with greater ease.
To give an example, (> miller cooper) can be unsatisfied while the preceding 4
constraints are satisfied.

(require (> miller cooper))
(require (not (= (abs (- fletcher cooper)) 1)))
(require (not (= (abs (- smith fletcher)) 1)))
(require (not (= cooper 1)))
(require (not (= fletcher 1)))
(require (not (= fletcher 5)))
(require (not (= baker 5)))

However, the re-ordering above still permits proposals which will fail on
single variable requirements. Thus, it will not be faster unless we could proceed
from the partial solutions generated by the multiple versions.

|#


;; Ex. 4.40
#|

Before distinct: 5^5 possible assignments
After distinct:  5! possible assignments

There are several ways to improve. First, we consider the creation of only the permutations.
Then, we consider a second improvement which exploits the structure of the problem.
|#
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5)))
    (let ((cooper (an-element-of (filter (lambda (x)
                                           (not (= baker x)))
                                         (list 1 2 3 4 5)))))
      (let ((fletcher (an-element-of (filter (lambda (x)
                                               (and (not (= baker x))
                                                    (not (= cooper x))))
                                             (list 1 2 3 4 5)))))
        (let ((miller (an-element-of (filter (lambda (x)
                                               (and (not (= baker x))
                                                    (not (= cooper x))
                                                    (not (= fletcher x))))
                                             (list 1 2 3 4 5)))))
          (let ((smith (an-element-of (filter (lambda (x)
                                                (and (not (= baker x))
                                                     (not (= cooper x))
                                                     (not (= fletcher x))
                                                     (not (= miller x))))
                                              (list 1 2 3 4 5)))))
            (require (not (= baker 5)))
            (require (not (= cooper 1)))
            (require (not (= fletcher 5)))
            (require (not (= fletcher 1)))
            (require (> miller cooper))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require (not (= (abs (- fletcher cooper)) 1)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))


(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4))) ;; do not even allow baker to be 5
    (let ((cooper (an-element-of (filter (lambda (x)
                                           (not (= baker x)))
                                         (list 2 3 4 5))))) ;; ensure distinct and eliminate cooper =1
      (let ((fletcher (an-element-of (filter (lambda (x)
                                               (and (not (= baker x))
                                                    (not (= cooper x))))
                                             (list 2 3 4))))) ;; ensure distinct and eliminate fletcher = 1 or 5
        (require (not (= (abs (- fletcher cooper)) 1))) ;; check before generating full list
        (let ((miller (an-element-of (filter (lambda (x)
                                               (and (not (= baker x))
                                                    (not (= cooper x))
                                                    (not (= fletcher x))))
                                             (list 1 2 3 4 5)))))
          (require (> miller cooper)) ;; check before generating full list
          (let ((smith (an-element-of (filter (lambda (x)
                                                (and (not (= baker x))
                                                     (not (= cooper x))
                                                     (not (= fletcher x))
                                                     (not (= miller x))))
                                              (list 1 2 3 4 5)))))
            (require (not (= (abs (- smith fletcher)) 1)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))


;; Ex. 4.41

#|
Not the most efficient approach, but works.
Easier to extend to the logic of Ex. 4.38 than the highly customized approach
of Ex. 4.40.
|#
(define (caddddr x) (car (cddddr x)))
(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))
(define (my-remove item seq)
  (filter (lambda (x) (not (= x item))) seq))
(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (my-remove x s))))
               s)))

(define (feasible-dwelling-assignment? dwelling-list)
  (let ((baker (car dwelling-list))
        (cooper (cadr dwelling-list))
        (fletcher (caddr dwelling-list))
        (miller (cadddr dwelling-list))
        (smith (caddddr dwelling-list)))
    (and (not (= baker 5))
         (not (= cooper 1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller cooper)
         (not (= (abs (- smith fletcher)) 1))
         (not (= (abs (- fletcher cooper)) 1)))))

(define (multiple-dwelling)
  (map (lambda (x)
         (list (list 'baker (car x))
               (list 'cooper (cadr x))
               (list 'fletcher (caddr x))
               (list 'miller (cadddr x))
               (list 'smith (caddddr x))))
       (filter feasible-dwelling-assignment?
               (permutations (list 1 2 3 4 5)))))
(multiple-dwelling)

;; Demonstrate ease of application to logic of Ex. 4.38
(define (feasible-dwelling-assignment-modified? dwelling-list)
  (let ((baker (car dwelling-list))
        (cooper (cadr dwelling-list))
        (fletcher (caddr dwelling-list))
        (miller (cadddr dwelling-list))
        (smith (caddddr dwelling-list)))
    (and (not (= baker 5))
         (not (= cooper 1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller cooper)
         (not (= (abs (- fletcher cooper)) 1)))))

(define (multiple-dwelling-modified)
  (map (lambda (x)
         (list (list 'baker (car x))
               (list 'cooper (cadr x))
               (list 'fletcher (caddr x))
               (list 'miller (cadddr x))
               (list 'smith (caddddr x))))
       (filter feasible-dwelling-assignment-modified?
               (permutations (list 1 2 3 4 5)))))
(multiple-dwelling-modified)

;; The generalization

(define (multiple-dwelling-arbitrary satisfies-constraints?)
  (map (lambda (x)
         (list (list 'baker (car x))
               (list 'cooper (cadr x))
               (list 'fletcher (caddr x))
               (list 'miller (cadddr x))
               (list 'smith (caddddr x))))
       (filter satisfies-constraints?
               (permutations (list 1 2 3 4 5)))))

;; We can then express the original and modified versions as:
(define multiple-dwelling
  (multiple-dwelling-arbitrary feasible-dwelling-assignment?))
(define multiple-dwelling-modified
  (multiple-dwelling-arbitrary feasible-dwelling-assignment-modified?))


;; Ex. 4.42

#|

Betty
_____
Betty : 3
Kitty : 2

Ethel
_____
Ethel : 1
Joan  : 2

Joan
____
Joan  : 3
Ethel : 5

Kitty
_____
Kitty : 2
Mary  : 4

Mary
____
Mary  : 4
Betty : 1


If Mary : 4 is true, then Kitty : 2 is false and Betty : 1 is false.
Thus, Betty : 3 is true. Thus, Joan : 3 cannot be true. Thus, Ethel : 5 is true.
Therefore, Ethel : 1 is false and Joan : 2 must be true. Consequently, one
deduces Kitty : 1.

Result
______
Kitty : 1
Joan  : 2
Betty : 3
Mary  : 4
Ethel : 5

Interestingly, one can invert the order of the statements and draw the inverted conclusion.

|#


;; Ex. 4.43

#|

Part 1

Father                Daughter                Yacht
______                ________                _____
Moore                 Mary Ann                Lorna
Downing               *Lorna                  Melissa
Hall                  *Gabrielle              Rosalind
Barnacle              Melissa                 Gabrielle
Parker                *Rosalind               **Mary Ann

** Simple deduction
* Inference
|#

(define (fathers-daughters-yachts)
  (let ((moore (list 'moore 'mary-ann 'lorna))
        (barnacle (list 'barnacle 'melissa 'gabrielle)))
    (let ((downing (list 'downing (amb 'lorna 'gabrielle 'rosalind) 'melissa)))
      (let ((daughters (filter (lambda (x)
                                 (not (eq? x (cadr downing))))
                               (list 'gabrielle 'lorna))))
        (let ((hall (list 'hall (an-element-of daughters) 'rosalind)))
          (let ((parker (list 'parker (an-element-of (filter (lambda (x)
                                                               (and (not (eq? x (cadr downing)))
                                                                    (not (eq? x (cadr hall)))))
                                                             (list 'lorna 'rosalind)))
                               'mary-ann)))
            (require (eq? (cadr parker)
                          (cond ((eq? 'gabrielle (cadr downing))
                                 (caddr downing))
                                ((eq? 'gabrielle (cadr hall))
                                 (caddr hall)))))
            (cond ((eq? 'lorna (cadr parker)) 'parker)
                  ((eq? 'lorna (cadr downing)) 'downing)
                  ((eq? 'lorna (cadr hall)) 'hall))))))))


#|

Part 2

Father                Daughter                Yacht
______                ________                _____
Moore                 *Gabrielle              Lorna
Downing               *Rosalind               Melissa
Hall                  *Mary Ann               Rosalind
Barnacle              Melissa                 Gabrielle
Parker                *Lorna                  **Mary Ann

** Simple deduction
* Inference

|#


(define (fathers-daughters-yachts)
  (let ((moore (list 'moore (amb 'gabrielle 'mary-ann 'rosalind) 'lorna))
        (barnacle (list 'barnacle 'melissa 'gabrielle)))
    (let ((downing (list 'downing (an-element-of (filter (lambda (x)
                                                           (not (eq? x (cadr moore))))
                                                         (list 'lorna 'gabrielle 'rosalind)))
                          'melissa)))
      (let ((hall (list 'hall (an-element-of (filter (lambda (x)
                                                       (and (not (eq? x (cadr moore)))
                                                            (not (eq? x (cadr downing)))))
                                                     (list 'gabrielle 'mary-ann 'lorna 'rosalind)))
                         'rosalind)))
        (let ((parker (list 'parker (an-element-of (filter (lambda (x)
                                                             (and (not (eq? x (cadr downing)))
                                                                  (not (eq? x (cadr hall)))))
                                                           (list 'lorna 'rosalind)))
                             'mary-ann)))
          (require (eq? (cadr parker)
                        (cond ((eq? 'gabrielle (cadr downing))
                               (caddr moore))
                              ((eq? 'gabrielle (cadr downing))
                               (caddr downing))
                              ((eq? 'gabrielle (cadr hall))
                               (caddr hall)))))
          (cond ((eq? 'lorna (cadr moore)) 'moore)
                ((eq? 'lorna (cadr downing)) 'downing)
                ((eq? 'lorna (cadr hall)) 'hall)
                ((eq? 'lorna (cadr parker)) 'parker)))))))



;; Ex. 4.44


;; Surprisingly terse compared to the version in Ex. 2.42
(define (queens board-size)
  (define (iter positions k)
    (if (not (= k board-size))
        (let ((new-positions
               (adjoin-position (an-integer-between 1 board-size) k positions)))
          (require (safe? k new-positions))
          (iter new-positions (+ k 1)))
        positions))
  (iter (list empty-board) 0))



;;;;;;;;;;;;;;;; Parsing natural language

;; Ex. 4.45

;; Parse #1
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase (prep to)
                (simple-noun-phrase (article the) (noun student))))
  (prep-phrase (prep in)
               (noun-phrase
                (simple-noun-phrase (article the) (noun class))
                (prep-phrase (prep with)
                             (simple-noun-phrase (article the) (noun cat)))))))

;; Parse #2
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb lectures)
   (prep-phrase (prep to)
                (noun-phrase
                 (simple-noun-phrase (article the) (noun student))
                 (prep-phrase (prep in)
                              (simple-noun-phrase (article the) (noun class))))))
  (prep-phrase (prep with)
               (simple-noun-phrase (article the) (noun cat)))))

;; Parse #3
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb lectures)
  (prep-phrase (prep to)
               (noun-phrase
                (simple-noun-phrase (article the) (noun student))
                (prep-phrase (prep in)
                             (noun-phrase
                              (simple-noun-phrase (article the) (noun class))
                              (prep-phrase (prep with)
                                           (simple-noun-phrase (article the) (noun cat)))))))))

;; Parse #4
(sentence
 (simple-noun-phrase (article the) (noun professor))
 (verb-phrase
  (verb-phrase
   (verb-phrase
    (verb lectures)
    (prep-phrase (prep to)
                 (simple-noun-phrase (article the) (noun student))))
   (prep-phrase (prep in)
                (simple-noun-phrase (article the) (noun class))))
  (prep-phrase (prep with)
               (simple-noun-phrase (article the) (noun cat)))))

;; Parse #5
;; ??


;; Ex. 4.46
#|
See p. 164 for a long but convoluted answer. Below is the simpler answer.

If we evaluated right-to-left, but followed the logic stated on p. 415-416,
then the selection of the first choice presented to amb would fail due to the
fact that the state of the parse has changed prior to the evaluation of amb
(thus, there is nothing to backtrack to but the next choice).
In essence, this precludes the possibility of taking the first choice for sentences
such as given in Ex.4.45 -- it is possible for the remnants of a sentence to be
parsable, but we still cannot avoid the problem that right-to-left evaluation
with the use of mutable state makes it impossible to visit some valid parses.
|#

#|
Actually, there is an even simpler answer still:
right-to-left evaluation everywhere would mean that in procedures
such as parse-prepositional-phrase and parse-sentence, that, respectively,
parse-noun-phrase and parse-verb-phrase would be evaluated first, at which time
the *unpaired* input would still contain a part of speed that is not, respectively,
a noun or verb phrase. Thus, the attempted evaluation will fail. This is particularly
poignant for parse-sentence, as our grammar is defined to be a noun-phrase followed by
verb-phrase. Attempting to parse the verb-phrase first will fail, initiating a
backtracking step, but there is nowhere to backtrack to -- this is the first
effective choice point. Specifically, (parse-word verbs) would be the first call;
it contains:
(require (not (null? *unparsed*)))                    ; always succeeds
(require (memq (car *unparsed*) (cdr word-list)))     ; failing choice point

Thus, we have an infinite loop:

         ^
         |
       0 |    +---------------------------+
         +----| (not (null? *unparsed*))  |----+
              +---------------------------+    | 1
                     ^                         |
                     |                         v
                  0  |   -+-----------------------------------------+
                     +----| (memq (car *unparsed*) (cdr word-list)) |----+
                          +-----------------------------------------+    | 1
                                                                         |
                                                                         v

The failure branch of the not null? machine can never be reached.
Likewise, the success branch of the memq machine is impossible as the
grammar begins with a noun phrase.
|#


;; Ex 4.47

#|
This does not work. Louis' proposal leads to the choice point being between
either a parse-word or a fully-formed list. If we take a parse-word branch,
the input will not be empty, thus we backtrack and try to construct a fully-formed
list. (parse-verb-phrase) will take the left branch, yielding a single verb,
and parse-prepositional-phrase will strip only 1 preposition (it takes left-branch).
However, the fully-formed list does not empty the input. Thus, we go back to the choice
point -- but we have exhausted our apparent options. We then backtrack to
parse-noun-phrase in parse-sentence, which succeeds, hence we re-enter parse-verb-phrase,
thereby forming an infinite loop.

If we interchange the order of expressions, then we get an infinite recursion, as the first
action of each step is parse-verb-phrase, which simply leads to a successful call of itself.
Since every amb is successful, (parse-word verbs) never occurs.
|#

;; Ex. 4.48
(define adjectives '(adjective small medium large))
(define adverbs ('adverb slowly rapidly))

(define (parse-adjective-noun-phrase)
  (list 'adjective-noun-phrase
         (parse-word articles)
         (parse-word adjectives)
         (parse-word nouns)))
(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrases
         (maybe-extend (list 'noun-phrase
                              noun-phrase
                              (parse-prepositional-phrase)))))
  (maybe-extend (amb (parse-simple-noun-phrase) (parse-adjective-noun-phrase))))

(define (parse-simple-verb-phrase)
  (parse-word verbs))
(define (parse-verb-adverb-phrase)
  (list (parse-word verbs)
        (parse-word adverbs)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                              verb-phrase
                              (parse-prepositional-phrase)))))
  ;; One should try to parse the verb-adverb phrase before the simple verb phrae
  ;; given the ordering of adverbs in relation to verbs.
  (maybe-extend (amb (parse-verb-adverb-phrse) (parse-simple-verb-phrase))))


;; Ex. 4.49
(define (parse-word word-list)
  (list (car word-list) (an-element-of (cdr word-list))))

;; The last choice point will be maybe-extend in parse-noun-phrase, for which
;; the success branch is always viable, leading to the same choice point.
;; Thus we get infinite repetition.
