;; 4.1.1 The Core of the Evaluator

;; Ex. 4.1

(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((left (eval (first-exp exps) env)))
        (let ((right (list-of-values-left-to-right (rest-exps exps) env)))
          (cons left right)))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((right (list-of-values-right-to-left (rest-exps exps) env)))
        (let ((left (eval (first-exp exps) env)))
          (cons left right)))))
