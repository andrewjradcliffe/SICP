;; Ex. 3.9

(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw initial-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 (list balance initial-amount))
          "Insufficient funds")))
   initial-amount))


(define W1 (make-withdraw 100))
(W1 50)
(W1 25)
