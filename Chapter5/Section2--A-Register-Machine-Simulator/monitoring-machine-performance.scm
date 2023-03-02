;; 5.2.4 Monitoring Machine Performance

;; Ex. 5.14

(define factorial-machine
  (make-machine
   '(continue n val)
   (list (list '* *) (list '= =) (list '- -))
   `((perform (op initialize-stack))
     ,@recursive-factorial-controller-text
     (perform (op print-stack-statistics)))))

(define (compute-factorial n)
  (set-register-contents! factorial-machine 'n n)
  (start factorial-machine)
  (get-register-contents factorial-machine 'val))

(define (factorial-interactive)
  (let ((n (read)))
    (newline)
    (display "(factorial ")
    (display n)
    (display ")")
    (let ((val (compute-factorial n)))
      (newline)
      (display "val = ")
      (display val)
      (newline)
      (newline)))
  (factorial-interactive))


;; (define (factorial n)
;;   (define (iter product counter)
;;     (if (> counter n)
;;         product
;;         (iter (* counter product) (+ counter 1))))
;;   (iter 1 1))

#|
In fact, initialize-stack is not automatically called anywhere.
However, we can use perform instructions to include it in the controller.
