;; 2.2.2. Hierarchical Structures

;; Ex. 2.24
(define l3 (list 1 (list 2 (list 3 4))))

(= (cadr (cadr (cadr l3))) 4)

;; Ex. 2.25
(define l225_1 (list 1 3 (list 5 7) 9))
(= (car (cdr (car (cdr (cdr l225_1))))) 7)
(= (cadr (cadr (cdr l225_1))) 7)

(define l225_2 (list (list 7)))
(= (car (car l225_2)) 7)
(= (caar l225_2) 7)

(define l225_3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(= (cadr (cadr (cadr (cadr (cadr (cadr l225_3)))))) 7)

;; Ex. 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; (1 2 3 4 5 6)
(cons x y)   ; ((1 2 3) 4 5 6)
(list x y)   ; ((1 2 3) (4 5 6))

;; Ex. 2.27

(define (deep-reverse items)
  (deep-reverse-impl items '()))

;; (define (deep-reverse-impl list1 list2)
;;   (cond ((null? list1) list2)
;;         ;; ((pair? list1)
;;         ;;  (cons (deep-reverse (cdr list1)) (cons (deep-reverse (car list1)) list2)))
;;         ;; (else (deep-reverse-impl (cdr list1) (cons (car list1) list2)))
;;         ((not (pair? list1))
;;          (cons list1 list2))
;;         ;; (else (deep-reverse-impl (cdr list1) (deep-reverse-impl (car list1) list2)))
;;         ;; (else (deep-reverse-impl (cdr list1) (cons (deep-reverse (car list1)) list2)))
;;         ;; (else (cons (deep-reverse-impl (cdr list1) (deep-reverse (car list1))) list2))
;;         (else (deep-reverse-impl (cdr list1) (cons (deep-reverse (car list1)) list2)))

;;         ))

(deep-reverse (list 1 2 3))


(define (deep-reverse-impl x y)
  (cond ((null? x) y)
        ((not (pair? x)) (cons x y))
        ((pair? (car x))
         (deep-reverse-impl (cdr x) (cons (deep-reverse (car x)) y)))
        (else (deep-reverse-impl (cdr x) (deep-reverse-impl (car x) y)))))

(define l227_1 (list (list 1 2) (list 3 4)))
(define l227_2 (list (list 4 3) (list 2 1)))

(deep-reverse l227_1)
(deep-reverse l227_2)
(deep-reverse (list l227_1 l227_2))
(deep-reverse (list l227_1 (list l227_2 5)))


;; Ex. 2.28
(define l228 (list (list 1 2) (list 3 4)))

(define (fringe x)
  (fringe-impl x '()))

(define (fringe-impl x y)
  (cond ((null? x) y)
        ((not (pair? x))
         (cons x y))
        ;; (else (fringe-impl (cdr x) (fringe-impl (car x) y)))
        (else (fringe-impl (car x) (fringe-impl (cdr x) y)))
        ))

(fringe l228)
(fringe (list l228 l228))

;; Ex. 2.29

;; Ex. 2.30

(define (square-tree t)
  (cond ((null? t) ())
        ((not (pair? t)) (* t t))
        (else (cons (square-tree (car t)) (square-tree (cdr t))))))

(define (square-tree t)
  (map (lambda (x)
         (if (pair? x)
             (square-tree x)
             (* x x)))
       t))

(square-tree (list 1
                   (list 2 (list 3 4) 5)
                   (list 6 7)))

;; Ex. 2.31

(define (tree-map proc t)
  (cond ((null? t) ())
        ((not (pair? t) (proc t)))
        (else (cons (tree-map proc (car t)) (tree-map proc (cdr t))))))

;; Alternate in terms of map
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree) (tree-map (lambda (x) (* x x)) tree))
(define (scale-tree tree) (tree-map (lambda (x) (* x factor)) tree))
