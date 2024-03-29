;; 3.2.3 Frames as the Repository of Local State

;; Ex. 3.9
;; See p. 50 for illustration

(define (make-withdraw-v1 initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw-v2 initial-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 ;; (list balance initial-amount) ;; demonstrate that E1 is visible and still exists
                 balance
                 )
          "Insufficient funds")))
   initial-amount))


(define W1-v1 (make-withdraw-v1 100))
(W1-v1 50)
(W1-v1 25)

(define W1-v2 (make-withdraw-v2 100))
(W1-v2 50)
(W1-v2 25)


;; Ex. 3.10
;; See p. 51, 52 for illustration

;; Ex. 3.11
;; See p. 53 for illustration

;; Ex. 3.12
;; See p. 53 for illustration
