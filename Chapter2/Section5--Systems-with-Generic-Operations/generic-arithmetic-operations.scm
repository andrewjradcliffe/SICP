;; 2.5.1 Generic Arithmetic Operations

;; 2.77
;; Why it works: defines the method for complex type. Recall:
;; (define (magnitude z) (apply-generic 'magnitude z))

;; The first call to apply-generic produces
;; ]=> type-tags := (complex)
;; which is then used to look up the procedure via (get op type-tags)
;; ]=> proc := magnitude (i.e. the implementation for complex)
;; As the procedure was found, it is then apply to the args:
;; (apply proc (map contents args))
;; This produces a call to the magnitude on the rectangular type,
;; which results in a second call to apply-generic:
;; Get type-tags: type-tags := (rectangular)
;; Attempt to find procedure: proc := magnitude (the implementation for rectangular)
;; Apply proc or error

;; apply-generic is invoked 2 times, if counting only the dispatches on magnitude.
;; The rectangular magnitude itself does not dispatch.

;; Ex. 2.78
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

;; Ex 2.79
(define (equ? x y) (apply-generic 'equ? x y))

;; within install-scheme-number-package
(put 'equ? '(scheme-number scheme-number) ;; = also works
     (lambda (x y) (= x y)))

;; within install-rational-package
(define (equ-rat? x y)
  (and (= (numer x) (numer y)) (= (denom x) (denom y))))
(put 'equ? '(rational rational) equ-rat?)

;; within install-complex-package
(define (equ-complex? x y)
  (and (= (real-part x) (real-part y)) (= (imag-part x) (imag-part y))))
(put 'equ? '(complex complex) equ-complex?)

;; Ex 2.80
(define (=zero? x) (apply-generic '=zero? x))

;; within install-scheme-number-package
(put '=zero? '(scheme-number) (lambda (x) (= x 0)))

;; within install-rational-package
(define (=zero-rat? x)
  (and (= (numer x) 0) (not (= (denom x) 0))))
(put '=zero? '(rational) =zero-rat?)

;; within install-complex-package
(define (=zero-complex? z)
  (and (= (real-part z) 0) (= (imag-part z) 0)))
(put '=zero? '(complex) =zero-complex?)
