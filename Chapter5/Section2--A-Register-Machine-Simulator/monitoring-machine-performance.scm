;; 5.2.4 Monitoring Machine Performance

;; Ex. 5.14

(define factorial-machine
  (make-machine
   '(continue n val)
   (list (list '* *) (list '= =) (list '- -))
   '((perform (op initialize-stack))
     (assign continue (label fact-done))
     fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
     after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     fact-done
     (perform op print-stack-statistics))))

(define (compute-factorial n)
  (set-register-contents! factorial-machine 'n n)
  (start factorial-machine)
  (get-register-contents factorial-machine 'val))

(define (factorial-interactive)
  (let ((n (read)))
    (newline)
    (display "(factorial ")
    (display n)
    (display ") = ")
    (let ((val (compute-factorial n)))
      (display val)
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
