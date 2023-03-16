;; 5.5.6 Lexical Addressing

(define (make-lexical-address f-num d-num) (list f-num d-num))
(define (frame-number lexical-address) (car lexical-address))
(define (displacement-number lexical-address) (cadr lexical-address))

;; Ex. 5.39
(define (lexical-address-lookup lexical-address runtime-env)
  (let ((f-num (frame-number lexical-address))
        (d-num (displacement-number lexical-address)))
    (let ((frame (list-ref runtime-env f-num)))
      (let ((val (list-ref (frame-values frame) d-num)))
        (if (eq? val '*unassigned*)
            (error "Unassigned variable"
                   (list-ref (frame-variables frame) d-num))
            val)))))

(define (lexical-address-set! lexical-address value runtime-env)
  (let ((f-num (frame-number lexical-address))
        (d-num (displacement-number lexical-address)))
    (let ((frame (list-ref runtime-env f-num)))
      (if (eq? (list-ref-set-car! (frame-values frame) d-num value) 'not-ok)
          (error "Bad lexical address" lexical-address runtime-env)
          'ok))))
(define (list-ref-set-car! seq n value)
  (if (null? seq)
      'not-ok
      (if (= n 0)
          (set-car! seq value)
          (list-ref-set-car! (cdr seq) (- n 1) value))))

;; Ex. 5.40
(define empty-compile-time-environment '())

