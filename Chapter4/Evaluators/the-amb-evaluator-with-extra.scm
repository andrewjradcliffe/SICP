;; Extension of the amb evaluator to include extensions from Section 4.3.3
#|
Includes the following special forms: ramb (that is, random branching), if-fail,
permanent-set!
|#

(load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/the-amb-evaluator.scm")

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ;;;;;;;;;;;;;;;; amb-related
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((if-fail? exp) (analyze (if-fail->amb exp)))
        ;;;;;;;;;;;;;;;; start of extra syntax
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((let*? exp) (analyze (let*->nested-lets exp)))
        ((letrec? exp) (analyze (letrec->let exp)))
        ;; Better handling of not than using implementation language primitive
        ((not? exp) (analyze (not->if exp)))
        ;;;;;;;;;;;;;;;; end of extra syntax
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))


(define (random-pop-and-rest items)
  (let ((n (length items)))
    (let ((i (random n)))
      (define (iter count old-seq new-seq)
        (if (and (< count n) (not (= i count)))
            (iter (+ count 1)
                  (cdr old-seq)
                  ;; if one wants to preserve the original order, then use:
                  (append new-seq (list (car old-seq)))
                  ;; else:
                  ;; (cons (car old-seq) new-seq)
                  )
            (if (= count n)
                new-seq
                (iter (+ count 1)
                      (cdr old-seq)
                      new-seq))))
      (cons (list-ref items i)
            (iter 0 items '())))))

(define (analyze-ramb exp)
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            (let ((p-r (random-pop-and-rest choices)))
              (let ((pop (car p-r))
                    (rest (cdr p-r)))
                (pop env
                     succeed
                     (lambda ()
                       (try-next rest)))))))
      (try-next cprocs))))

(define (ramb? exp) (tagged-list? exp 'ramb))
(define (ramb-choices exp) (cdr exp))


(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-normal exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))
(define (if-fail->amb exp) (cons 'amb (cdr exp)))

;;;;;;;;;;;;;;;; Tests
(driver-loop)
(define (require p)
  (if (not p) (amb)))
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))
(define (a-random-element-of items)
  (require (not (null? items)))
  (ramb (car items) (a-random-element-of (cdr items))))
(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

;;;; Ex. 4.44
(define empty-board '())
(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (list new-row k))))

(define (same-row? x y) (= (car x) (car y)))
(define (same-col? x y) (= (cadr x) (cadr y)))
(define (same-diagonal? x y)
  (let ((di (- (car x) (car y)))
        (dj (- (cadr x) (cadr y))))
    (= (abs di) (abs dj))))
(define (safe-or-identical? x y)
  (if (equal? x y)
      true
      (and (not (same-row? x y)) (not (same-col? x y)) (not (same-diagonal? x y)))))
;; (safe-or-identical? '(1 1) '(2 3))
(define (safe? k positions)
  (let ((queen (list-ref positions k)))
    (accumulate (lambda (x y) (and (safe-or-identical? queen x) y))
                true
                positions)))

(define (display-queen-row k row)
  (for-each (lambda (x) (display (if (= x k) "■\t" "⋅\t"))) row)
  (newline))
(define (display-queens positions)
  (newline)
  (display positions)
  (newline)
  (newline)
  (let ((row (car (transpose positions))))
    (for-each (lambda (k) (display-queen-row k row)) (enumerate-interval 1 (length row)))))

(display-queens (queens 8))

;;;;;;;;;;;;;;;; Parsing natural language
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

(define *unparsed* '())
(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))
(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (ramb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (ramb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))


(parse '(the professor lectures to the student in the class with the cat))


(define (parse-word word-list)
  ;; These can be used to exert control on size of the generated sentence.
  ;; (require (not (null? *unparsed*)))
  ;; (set! *unparsed* (cdr *unparsed*))
  (list (car word-list) (a-random-element-of (cdr word-list))))
;; (parse '(a b c d e f))
