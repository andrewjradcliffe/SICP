;; 3.1.1 Local State Variables

;; Ex. 3.1

(define (make-accumulator sum)
  (lambda (x)
    (begin (set! sum (+ sum x))
           sum)))

(define A (make-accumulator 5))
(A 10)
(A 10)

;; Ex. 3.2

(define (make-monitored f)
  (let ((n 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) n)
            ((eq? x 'reset-count)
             (begin (set! n 0)
                    n))
            (else (begin (set! n (+ n 1))
                         (f x)))))))

;; Or, a similar approach to introducing an environment which contains n
(define (make-monitored f)
  (define (mf n)
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) n)
            ((eq? x 'reset-count)
             (begin (set! n 0)
                    n))
            (else (begin (set! n (+ n 1))
                         (f x))))))
  (mf 0))

(define s (make-monitored sqrt))
(s 100)
(s 'how-many-calls?)
(s 'reset-count)

;; Ex. 3.3

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (define (dispatch pw m)
    (if (eq? pw password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        (lambda (x) "Incorrect password")))
  dispatch)

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)

;; Ex. 3.4

(define (call-the-cops x)
  "Calling the cops -- > 7 attempts with incorrect password")

(define (incorrect-password x) "Incorrect password")

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (begin (set! balance (+ balance amount))
           balance))
  (let ((n 0))
    (define (dispatch pw m)
      (if (eq? pw password)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT" m)))
          (begin (set! n (+ n 1))
                 (if (> n 7) call-the-cops incorrect-password))))
    dispatch))

(define acc (make-account 100 'secret-password))

((acc 'secret-password 'withdraw) 40)

((acc 'some-other-password 'deposit) 50)
