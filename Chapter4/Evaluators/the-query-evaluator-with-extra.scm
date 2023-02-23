;; The evaluator of 4.4.4 based on a logic programming language for queries,
;; with additions from 4.75
#|
This is a pure addition to the base query evaluator.
|#
(load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/the-query-evaluator.scm")

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
(put 'unique 'qeval uniquely-asserted)

;;;;;;;;;;;;;;;; Tests
(query-driver-loop)

;; ;; After loading microshaft items
;; (unique (job ?x (computer wizard)))                ;; prints 1
;; (unique (job ?x (computer programmer)))            ;; prints empty stream
;; (and (job ?x ?j) (unique (job ?anyone ?j)))        ;; prints 6
;; (and (supervisor ?x ?y)                            ;; prints 2
;;      (unique (supervisor ?z ?y)))
