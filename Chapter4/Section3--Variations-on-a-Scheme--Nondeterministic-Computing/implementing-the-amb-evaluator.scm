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

