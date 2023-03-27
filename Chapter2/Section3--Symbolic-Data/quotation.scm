;; 2.3.1 Quotation

;; Ex. 2.53
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; false
(memq 'red '((red shoe) (blue shoe))) ; false
(memq 'red '(red shoes blue socks)) ; (red shoes blue socks)

;; Ex. 2.54
(define (equal? a b)
  (and (not (null? a))
       (not (null? b))
       (eq? (car a) (car b))
       (equal? (cdr a) (cdr b))))

;; Ex. 2.55
(car ''abracadabra) ; (car (quote (quote abracadabra))) ; (car '(quote abracadabra))
