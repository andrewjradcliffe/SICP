;; 2.1.3. What is Meant by Data?

;; Ex. 2.4

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))


;; Ex. 2.5

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car z)
  (find-a z))

(define (cdr z)
  (find-b z))

(define (find-a z)
  (define (iter x a)
    (if (= (rem z x) 0)
        (iter (* x 2) (+ a 1))
        a))
  (iter 1 0))

(define (find-b z)
  (define (iter x a)
    (if (= (rem z x) 0)
        (iter (* x 3) (+ a 1))
        a))
  (iter 1 0))


;; Ex. 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (add-1 zero)) ;; the trivial definition

(define one  ;; the proper definition
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

(define three
  (lambda (f) (lambda (x) (f (f (f x))))))

(define (+ a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
