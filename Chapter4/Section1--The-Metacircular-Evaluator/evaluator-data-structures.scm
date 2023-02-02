;; 4.1.3 Evaluator Data Structures

;; Ex. 4.11

(define (make-frame variables values)
  (define (iter vars vals)
    (if (null? vars)
        '()
        (cons (cons (car vars) (car vals))
              (iter (cdr vars) (cdr vals)))))
  (iter variables values))

(define (frame-variables frame)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (caar bindings)
              (iter (cdr bindings)))))
  (iter frame))

(define (frame-values frame)
  (define (iter bindings)
    (if (null? bindings)
        '()
        (cons (cdar bindings)
              (iter (cdr bindings)))))
  (iter frame))

(define (extend-environment vars vals base-env) ;; same
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings))
             (cdar bindings))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (scan (first-frame env)))
    (env-loop env)))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan bindings)
      (cond ((null? bindings)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar bindings))
             (set-cdr! (car bindings) val))
            (else (scan (cdr bindings)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (scan (first-frame env))))
  (env-loop env))

(define (define-variable! var val env)
  (define (scan bindings)
    (cond ((null? bindings)
           (set-car! env (cons (cons var val)
                               (first-frame env))))
          ((eq? var (caar bindings))
           (set-cdr! (car bindings) val))
          (else (scan (cdr bindings)))))
  (scan (first-frame env)))


;; Ex. 4.12

;; if found, returns list of values with desired val at first position;
;; otherwise, returns empty list
(define (scan-frame var frame)
  (define (iter vars vals)
    (cond ((null? vars)
           '())
          ((eq? var (car vars))
           vals)
          (else (iter (cdr vars) (cdr vals)))))
  (iter (frame-variables frame)
        (frame-values frame)))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((vals (scan-frame var frame)))
      (if (null? vars)
          (add-binding-to-frame! var val frame)
          (set-car! vals val)))))

;; convenience predicates
(define (bound-in-frame? var frame)
  (not (null? (scan-frame var frame))))
(define (the-empty-environment? env)
  (eq? env the-empty-environment))

;; Version 1: recursively call the procedure itself
(define (lookup-variable-value var env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((vals (scan-frame var (first-frame env))))
        (if (null? vals)
            (lookup-variable-value var (enclosing-environment env))
            (car vals)))))

(define (set-variable-value! var val env)
  (if (eq? env the-empty-environment)
      (error "Unbound variable -- SET!" var)
      (let ((vals (scan-frame var (first-frame env))))
        (if (null? vals)
            (set-variable-value! var val (enclosing-environment env))
            (set-car! vals val)))))

;; Version 2: find the first environment in which the variable is bound (if at all),
;; then write procedures around this -- has the disadvantage of scanning
;; the containing environment twice (if it is bound).

(define (find-first-environment var env)
  (cond ((eq? env the-empty-environment)
         env)
        ((not (null? (scan-frame var (first-frame env))))
         env)
        (else (find-first-environment var (enclosing-environment env)))))

(define (lookup-variable-value var env)
  (let ((potential-env (find-first-environment var env)))
    (if (eq? potential-env the-empty-environment)
        (error "Unbound variable" var)
        (car (scan-frame var (first-frame potential-env))))))

(define (set-variable-value! var val env)
  (let ((potential-env (find-first-environment var env)))
    (if (eq? potential-env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (set-car! (scan-frame var (first-frame potential-env)) val))))

;; Version 3: abstract the action around the bound variable

(define (action-variable-value proc err var env)
  (let ((potential-env (find-first-environment var env)))
    (if (eq? potential-env the-empty-environment)
        (err var)
        (proc (scan-frame var (find-first potential-env))))))

(define (lookup-variable-value var env)
  (action-variable-value car
                         (lambda (x) (error "Unbound variable" x))
                         var
                         env))

(define (set-variable-value! var val env)
  (action-variable-value (lambda (vals) (set-car! vals val))
                         (lambda (x) (error "Unbound variable" x))
                         var
                         env))


;; Ex. 4.13
;; (make-unbound! <var>)
#|
Options:
1. un-bind in the environment passed as argument
2. un-bind in the environment passed as argument and all reachable environments
3. un-bind in first environment in which variable

Discussion:

1. Limiting scope to a single environment makes this closer to the behavior of define
(well, its inverse). This would also limit unintended consequences, and/or surprising
behavior.

2. On the other hand, set! acts recursively. However, set! on a variable which is
already bound, and throws an error instead of what would otherwise be a no-op
(i.e. an attempt to set! on an unbound variable). In contrast to 3., this is
at least predictable behavior.

3. This is unusual behavior, as it leads to the possibility of code that can
easily have unintended consequences -- problems arise quickly. A caller may
remove variables from an enclosing environment, but the procedures being
evaluated in that environment may depend on said variable being bound.
In essence, this too easily wrecks enclosing environment(s) to be of any
practical (or desirable!) use. Quite literally, this would require callers of
procedures to know of every occurrence of make-unbound! and avoid using the same
variable name.

Thus, there is only one potentially useful choice: Option 1. It provides an undefine!-like
concept, and, if one really wanted Option 2., one could always implement it by
calling make-unbound! on all enclosing environments.

However, one must always consider that since we can call define or set!,
why would we truly wish to call make-unbound! -- whatever happens to be bound
to the variables (which are no longer in use) is essentially harmless.

|#

(define (make-unbound!? exp) (tagged-list? exp 'make-unbound!))
(define (variable-to-unbind exp) (cadr exp))
(define (eval-unbind exp env)
  (unbind-variable! (variable-to-unbind exp) env))

(define (unbind-variable! var env)
  (define (scan vars vals)
    (cond ((null? (cdr vars))
           false) ;; arbitrary return value when variable not bound already
          ((eq? var (cadr vars))
           (let ((rest-vars (cddr vars))
                 (rest-vals (cddr vals)))
             (set-cdr! vars rest-vars)
             (set-cdr! vals rest-vals)))
          (else (scan (cdr vars) (cdr vals)))))
  (let ((frame (first-frame env)))
    (let ((vars (frame-variables frame))
          (vals (frame-values frame)))
      (cond ((null? vars)
             false)
            ((eq? var (car vars))
             (let ((rest-vars (cdr vars))
                   (rest-vals (cdr vals)))
               (set-car! frame rest-vars)
               (set-cdr! frame rest-vals)))
            (else (scan vars vals))))))
