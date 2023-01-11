;; 3.3.2 Representing Queues

;; Ex. 3.21
(define (print-queue queue)
  (newline)
  (display (front-ptr queue))
  (newline))

;; Ex. 3.22

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! item) (set! front-ptr item))
    (define (set-rear-ptr! item) (set! rear-ptr item))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue" (cons front-ptr rear-ptr))
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               dispatch))))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE called with an empty queue" (cons front-ptr rear-ptr)))
            (else
             (set-front-ptr! (cdr front-ptr))
             dispatch)))
    (define (dispatch m)
      (cond ((eq? m 'front-ptr) front-ptr)
            ((eq? m 'rear-ptr) rear-ptr)
            ((eq? m 'set-front-ptr!) set-front-ptr!)
            ((eq? m 'set-rear-ptr!) set-rear-ptr!)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'empty-queue?) empty-queue?)
            (else (error "Unknown method -- MAKE-QUEUE"))))
    dispatch))

;; External methods
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) ((queue 'delete-queue!)))
(define (set-front-ptr-queue! queue item) ((queue 'set-front-ptr!) item))
(define (set-rear-ptr-queue! queue item) ((queue 'set-rear-ptr!) item))
(define (empty-queue? queue) ((queue 'empty-queue?)))
(define (front-queue queue) ((queue 'front-queue)))
(define (front-ptr-queue queue) (queue 'front-ptr))
(define (rear-ptr-queue queue) (queue 'rear-ptr))

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(delete-queue! q1)
(delete-queue! q1)

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
  (display (collect-deque deque))
  (newline))

;; front-only tests
(define q1 (make-deque))

(front-insert-deque! q1 'a)
(front-insert-deque! q1 'b)
(front-insert-deque! q1 'c)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)
(front-delete-deque! q1)
(print-deque q1)

;; rear-only tests
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

;; mixed tests
(define q3 (make-deque))
(rear-insert-deque! q3 'd)
(front-insert-deque! q3 'a)
(front-insert-deque! q3 'b)
(print-deque q3)
(rear-insert-deque! q3 '?)
(print-deque q3)
