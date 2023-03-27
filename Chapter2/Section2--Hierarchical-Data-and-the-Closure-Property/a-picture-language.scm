;; 2.2.4 Example: A Picture Language

;; Ex. 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;; Ex. 2.45
(define (split outer inner)
  (lambda (painter n)
    (if (= n 0)
        painter
        (let ((smaller ((split outer inner) painter (- n 1))))
          (outer painter (inner smaller smaller))))))
(define right-split (split beside below))
(define left-split (split below beside))

;; Ex. 2.46
(define (make-vect x y)
  (list x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cadr v))
(define (add-vect v w)
  (map + v w))
(define (sub-vect v w)
  (map - v w))
(define (scale-vect s v)
  (map (lambda (x) (* s x)) v))

;; Ex. 2.47
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (cadr frame))
(define (edge2-frame frame) (caddr frame))

;; Ex. 2.48
(define (make-segment start end)
  (cons start end))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

;; Ex. 2.49

;; a
(define (outline-painter f)
  ((segments->painter
    (list
     ;; bottom-left    bottom-right
     (make-segment (make-vect 0 0) (make-vect 1 0))
     ;; bottom-left    top-left
     (make-segment (make-vect 0 0) (make-vect 0 1))
     ;; top-left       top-right
     (make-segment (make-vect 0 1) (make-vect 1 1))
     ;; top-right      bottom-right
     (make-segment (make-vect 1 1) (make-vect 1 0))))
   f))

;; b
(define (x-painter f)
  ((segments->painter
    (list
     ;; bottom-left    top-right
     (make-segment (make-vect 0 0) (make-vect 1 1))
     ;; bottom-right   top-left
     (make-segment (make-vect 1 0) (make-vect 0 1))))
   f))

;; c
(define (diamond-painter f)
  ((segments->painter
    (list
     (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
     (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
     (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
     (make-segment (make-vect 0 0.5) (make-vect 0.5 0))))
   f))

;; d
#|
The image created does not have identical proportions to the wave in SICP,
but it should be reasonably close.
|#
(define (wave f)
  ((segments->painter
    (list
     ;; bottom triangle
     (make-segment (make-vect 0.4 0) (make-vect 0.5 0.33))
     (make-segment (make-vect 0.5 0.33) (make-vect 0.6 0))
     ;; lower left
     (make-segment (make-vect 0 0.65) (make-vect 0.15 0.4))
     (make-segment (make-vect 0.15 0.4) (make-vect 0.25 0.6))
     (make-segment (make-vect 0.25 0.6) (make-vect 0.33 0.5))
     (make-segment (make-vect 0.33 0.5) (make-vect 0.25 0))
     ;; upper left
     (make-segment (make-vect 0 0.8) (make-vect 0.15 0.6))
     (make-segment (make-vect 0.15 0.6) (make-vect 0.3 0.65))
     (make-segment (make-vect 0.3 0.65) (make-vect 0.4 0.65))
     (make-segment (make-vect 0.4 0.65) (make-vect 0.35 0.8))
     (make-segment (make-vect 0.35 0.8) (make-vect 0.4 1))
     ;; upper right
     (make-segment (make-vect 0.65 1) (make-vect 0.7 0.8))
     (make-segment (make-vect 0.7 0.8) (make-vect 0.6 0.65))
     (make-segment (make-vect 0.6 0.65) (make-vect 0.8 0.65))
     (make-segment (make-vect 0.8 0.65) (make-vect 1 0.4))
     ;; lower-right
     (make-segment (make-vect 1 0.25) (make-vect 0.6 0.45))
     (make-segment (make-vect 0.6 0.45) (make-vect 0.75 0))))
   f))


;; Ex. 2.50
#|
See p. 23 for illustrative drawings and the linear algebra to back this up.
|#
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 0 0)
                     (make-vect 0 1)))

;; The lazy way
(define (rotate180 painter)
  (rotate90 (rotate90 painter)))
(define (rotate270 painter)
  (rotate90 (rotate180 painter)))

;; The rigorous way
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1 1)
                     (make-vect 0 1)
                     (make-vect 1 0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0 1)
                     (make-vect 0 0)
                     (make-vect 1 1)))

;; Ex. 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0 0.5)))
    (let ((paint-bottom
           (transform-painter painter1
                              (make-vect 0 0)
                              (make-vect 1 0)
                              split-point))
          (paint-top
           (transform-painter painter2
                              split-point
                              (make-vect 1 0.5)
                              (make-vect 0 1))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)))))

;; Or, simpler:
(define (below painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))

;; Ex. 2.52

;; a
;; addition of smile
(make-segment (make-vect 0.45 0.9) (make-vect 0.45 0.85))
(make-segment (make-vect 0.6 0.9) (make-vect 0.6 0.85))
(make-segment (make-vect 0.45 0.8) (make-vect 0.5 0.7))
(make-segment (make-vect 0.5 0.7) (make-vect 0.55 0.7))
(make-segment (make-vect 0.55 0.7) (make-vect 0.6 0.8))

;; b
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))

;; c
(define (square-limit painter n)
  (let ((combine4 (square-of-four identity flip-horiz flip-vert rotate180)))
    (combine4 (corner-split painter n))))
