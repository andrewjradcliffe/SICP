;; 5.2.4 Monitoring Machine Performance

;; Ex. 5.14

(define recursive-factorial-machine
  (make-machine
   '(continue n val)
   (list (list '* *) (list '= =) (list '- -))
   `((perform (op initialize-stack))
     ,@recursive-factorial-controller-text
     (perform (op print-stack-statistics)))))

(define (compute-recursive-factorial n)
  (set-register-contents! recursive-factorial-machine 'n n)
  (start recursive-factorial-machine)
  (get-register-contents recursive-factorial-machine 'val))

(define (recursive-factorial-interactive)
  (let ((n (read)))
    (newline)
    (display "(factorial ")
    (display n)
    (display ")")
    (let ((val (compute-recursive-factorial n)))
      (newline)
      (display "val = ")
      (display val)
      (newline)
      (newline)))
  (recursive-factorial-interactive))


;; (define (factorial n)
;;   (define (iter product counter)
;;     (if (> counter n)
;;         product
;;         (iter (* counter product) (+ counter 1))))
;;   (iter 1 1))

#|
In fact, initialize-stack is not automatically called anywhere.
However, we can use perform instructions to include it in the controller.

n total-pushes maximum-depth
2 2            2
3 4            4
4 6            6
.
.
.

2(n-1) = 2n - 2
|#
