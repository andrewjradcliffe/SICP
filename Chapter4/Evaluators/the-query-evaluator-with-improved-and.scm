;; The evaluator of 4.4.4 based on a logic programming language for queries,
;; with improvement to and special form from Ex. 4.76.
#|
This should be a transparent modification to the base query evaluator.
|#
(load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/the-query-evaluator.scm")

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
        (let ((merge-result (merge-if-compatible frame1 frame2)))
          (if (eq? merge-result 'failed)
              the-empty-stream
              (singleton-stream merge-result))))
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
                 'failed
                 (merge-if-compatible (cdr frame1) extended-frame)))))))
(put 'and 'qeval conjoin-improved)

;;;;;;;;;;;;;;;; Tests
(query-driver-loop)

;; ;; After loading microshaft items
;; (and (supervisor ?x (Bitdiddle Ben))                ;; prints 3
;;      (address ?x ?y))
;; (and (salary (Bitdiddle Ben) ?amount)               ;; prints 6
;;      (salary ?person ?other-amount)
;;      (lisp-value < ?other-amount ?amount))
;; (and (supervisor ?person ?super)                    ;; prints 1
;;      (job ?super ?division)
;;      (not (job ?super (computer . ?type))))
