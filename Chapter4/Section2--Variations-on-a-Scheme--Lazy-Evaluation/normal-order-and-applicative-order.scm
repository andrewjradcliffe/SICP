;; 4.2.1 Normal Order and Applicative Order

;; Ex. 4.25

#|
In applicative-order Scheme, the strict evaluation of arguments requires that all 3
(predicate, usual-value, exceptional-value) be evaluated before being passed to unless.
Thus, even a call of (factorial 1) will not terminate, as the expression
(* n (factorial (- n 1))) will be evaluated, thereby initiating another evaluation
of the same expression, etc. This would work in a normal-order language as the predicate
will evaluated before the other expressions, which are only then conditionally evaluated.
|#

;; Ex. 4.26

#|

If unless is implemented as a special form, then it's merely a trivial variation
on the "if" special form.


 (unless <condition>
   <usual-value>
   <exceptional-value>)

Transforms to:

 (if <condition>
     <exceptional-value>
     <usual-value>)

|#

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-condition exp) (cadr exp))
(define (unless-usual-value exp) (caddr exp))
(define (unless-exceptional-value exp) (cadddr exp))
(define (make-unless condition usual-value exceptional-value)
  (list 'unless condition usual-value exceptional-value))
(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-exceptional-value exp)
           (unless-usual-value exp)))

;; within eval:
((unless? exp) (eval (unless->if exp) env))
;; or, within analyze:
((unless? exp) (analyze (unless->if exp)))

#|

If unless is a procedure, then it can be passed as an argument to higher-order procedures.
A pathetic example:

(define (f proc a b c) (proc a b c))
(f unless 'x 'y 'z)    ;; z
(f list 'x 'y 'z)      ;; (x y z)

|#
