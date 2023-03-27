;; Ex. 2.3.2 Symbolic Differentiation

;; Ex. 2.56
(define (** b e) (expt b e))
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation b e)
  (list '** b e))

;; clause in deriv, to be inserted after product clause
((exponentiation? exp)
 (make-product (exponent exp)
               (make-product (make-exponentiation (base exp) (- (exponent exp) 1))
                             (deriv (base exp) var))))

;; fancier make-exponentiation which handles x^0 = 1 and x^1 = x
(define (make-exponentiation b e)
  (if (number? e)
      (if (= e 0) 1 b)
      (list '** b e)))

;; Ex. 2.57
(define (augend e)
  ;; (if (null? (cdddr e))
  ;;     (caddr e)
  ;;     (accumulate make-sum 0 (cddr e)))
  ;; can be shortened to just:
  (accumulate make-sum 0 (cddr e))
  )

(define (multiplicand e)
  (accumulate make-product 1 (cddr e)))

;; Ex. 2.58

;; a
(define (infix-arg1 e) (car e))
(define (infix-op e) (cadr e))
(define (infix-arg2 e) (caddr e))
(define (expression? x)
  (not (or (number? x) (symbol? x))))

(define (infix->prefix e)
  (let ((arg1 (infix-arg1 e))
        (op (infix-op e))
        (arg2 (infix-arg2 e)))
    (list op
          (if (or (number? arg1) (symbol? arg1))
              arg1
              (infix->prefix arg1))
          (if (or (number? arg2) (symbol? arg2))
              arg2
              (infix->prefix arg2)))))

(infix->prefix '(x + ((3 * x) + (y + 2))))


;; b
(define (literal-or-infix? x)
  (or (variable? x) (number? x) (infix? x)))
(define (ok-op? x)
  (or (eq? '+ x)
      (eq? '* x)
      (eq? '** x)))
(define (literal? x)
  (or (variable? x) (number? x)))

(define (infix? e)
  (if (null? e)
      true
      (let ((arg1 (infix-arg1 e))
            (op (infix-op e))
            (rest (cddr e)))
        (and (literal-or-infix? arg1)
             (ok-op? op)
             (literal-or-infix? (car rest))
             (infix? (cdr rest))))))

(define (standard->infix e)
  (if (infix? e)
      e
      (let ((arg1 (infix-arg1 e))
            (op (infix-op e))
            (rest (cddr e)))
        (if (eq? op '*)
            (let ((current
                   (list
                    (if (literal? arg1)
                        arg1
                        (standard->infix arg2))
                    '*
                    (if (literal? (car rest))
                        (car rest)
                        (standard->infix (car rest))))))
              (if (null? (cdr rest))
                  current
                  (standard->infix
                   (append (list current (cadr rest))
                           (cddr rest)))))
            (list
             (if (literal? arg1)
                 arg2
                 (standard->infix arg1))
             op
             (if (null? (cdr rest))
                 (if (literal? (car rest))
                     (car rest)
                     (standard->infix (car rest)))
                 (standard->infix rest)))
            ))))
