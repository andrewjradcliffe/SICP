;; 3.3.2 Representing Queues

;; Ex. 3.23

(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))

(define (make-deque) (cons '() '()))

(define (empty-deque? deque) (and (null? (front-ptr deque)) (null? (front-ptr deque))))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with empty deque" deque)
      (caar (front-ptr deque))))

(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with empty deque" deque)
      (caar (rear-ptr deque))))

(define (front-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((bot (cons item '())))
           (let ((top (cons bot '())))
             (set-front-ptr! deque top)
             (set-rear-ptr! deque top)
             deque)))
        (else
         (let ((bot (cons item '())))
           (let ((top (cons bot '())))
             (set-cdr! top (front-ptr deque))
             (set-cdr! (car (front-ptr deque)) top)
             (set-front-ptr! deque top)
             deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE called with empty deque" deque))
        (else
         (cond ((null? (cdr (front-ptr deque)))
                (set-front-ptr! deque '())
                (set-rear-ptr! deque '())
                deque)
               (else
                (set-cdr! (cadr (front-ptr deque)) '())
                (set-front-ptr! deque (cdr (front-ptr deque)))
                deque)))))

(define (rear-insert-deque! deque item)
  (cond ((empty-deque? deque)
         (let ((bot (cons item '())))
           (let ((top (cons bot '())))
             (set-front-ptr! deque top)
             (set-rear-ptr! deque top)
             deque)))
        (else
         (let ((bot (cons item (rear-ptr deque))))
           (let ((top (cons bot '())))
             (set-cdr! (rear-ptr deque) top)
             (set-rear-ptr! deque top)
             deque)))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE called with empty deque" deque))
        (else
         (cond ((null? (cdr (front-ptr deque)))
                (set-front-ptr! deque '())
                (set-rear-ptr! deque '())
                deque)
               (else
                (let ((previous (cdar (rear-ptr deque))))
                  (set-cdr! previous '())
                  (set-rear-ptr! deque previous)))))))

(define (collect-deque deque) (map car (front-ptr deque)))
(define (print-deque deque)
  (newline)
  (display (collect-deque))
  (newline))

(define q1 (make-deque))

(front-insert-deque! q1 'a)
(front-insert-deque! q1 'b)
(front-insert-deque! q1 'c)
(print-deque q1)

(define q2 (make-deque))
(rear-insert-deque! q2 'a)
(rear-insert-deque! q2 'b)
(rear-insert-deque! q2 'c)
(print-deque q2)
(rear-delete-deque! q2)
(print-deque q2)
(rear-delete-deque! q2)
(print-deque q2)
(rear-delete-deque! q2)
(print-deque q2)
;; Should error
(rear-delete-deque! q2)
(front-delete-deque! q2)
