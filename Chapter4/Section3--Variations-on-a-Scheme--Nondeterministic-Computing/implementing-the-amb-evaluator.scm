;; 4.3.3 Implementing the Amb Evaluator

;; Ex. 4.50
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
    (lambda (env success fail)
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

;; add to analyze:
((ramb? exp) (analyze-ramb exp))

;; We should be able to help with the problem in Ex. 4.49 by defining
;; a-random-element-of and substituting it for an-element-of
(define (a-random-element-of items)
  (require (not (null? items)))
  (ramb (car items) (a-random-element-of (cdr items))))

#|
This will generate less repetitive sentences, but they will still get stuck
in the infinite repetition of prepositional phrases following the verb phrase.
To produce truly random sentence structure, one need simply substitute ramb
for amb in parse-verb-phrase and parse-noun-phrase. The sentence will still
be a noun phrase followed by a verb phrase, but the article, noun and verb chosen
will span the space, and the prepositional phrase will be of random length.
|#

;; Ex. 4.51

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

;; within analyze, after regular assignment
((permanent-assignment? exp) (analyze-permanent-assignment exp))

#|
If set! had been used instead of permanent-set!, then the values displayed
would have been:
(a b 1)
(a c 1)
|#

;; Ex. 4.52
#|
The following implements if-fail as a syntactic transformation, but
there are other possible approaches.
|#
(define (if-fail? exp) (tagged-list? exp 'if-fail))
(define (if-fail-normal exp) (cadr exp))
(define (if-fail-alternative exp) (caddr exp))
(define (if-fail->amb exp) (cons 'amb (cdr exp)))

;; within analyze, prior to application?
((if-fail? exp) (analyze (if-fail->amb exp)))


;; Ex. 4.53
#|
pairs, as constructed by the sequence of assignments:
'() ----> ((3 20)) ----> ((3 110) (3 20)) ----> ((8 35) (3 110) (3 20))

If if-fail is implemented as a simple syntactic transformation to amb,
the lookup-variable-value call on the variable pairs in the environment
constructed by the lambda of the let will find and return ((8 35) (3 110) (3 20)).
|#

;; Ex. 4.54
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value)) ;; equivalent to (false? pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
