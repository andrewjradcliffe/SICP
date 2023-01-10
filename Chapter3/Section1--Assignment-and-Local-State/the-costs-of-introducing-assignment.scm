;; 3.1.3 The Costs of Introducing Assignment

;; Ex. 3.7

(define (make-joint account password new-password)
  (if (correct-password? account password)
      (lambda (pw m)
        (if (eq? pw new-password)
            (account password m)
            (lambda (x) "Incorrect password")))
      (error "Incorrect password for original account" (list account password))))

(define (correct-password? account password)
  (not (equal? ((account password 'deposit) 0) "Incorrect password")))

(define peter-acc (make-account 100 'open-sesame))

(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))

((paul-acc 'rosebud 'deposit) 10)
((peter-acc 'open-sesame 'deposit) 0)

;; Demonstrate that it works for any level of joining (i.e. more recursive calls)
(define my-acc
  (make-joint paul-acc 'rosebud 'citizen-kane))

((my-acc 'citizen-kane 'withdraw) 10)
((my-acc 'citizen-kan 'withdraw) 10)

(define my-acc-2
  (make-joint my-acc 'citizen-kane 'orson-wells))

((my-acc-2 'orson-wells 'deposit) 0)


;; Ex. 3.8

(define f
  (let ((n 0))
    (lambda (x)
      (cond ((= x n)
             (set! n 1)
             0)
            ((not (= x n))
             (set! n 0)
             1)))))
;; fresh define
(f 0) ; 0
(f 1) ; 0
;; then re-define
(f 1) ; 1
(f 0) ; 0

(+ (f 0) (f 1)) ; 1, suggests that interpreter selects right-to-left
(+ (f 1) (f 0)) ; 0, suggests that interpreter selects right-to-left
