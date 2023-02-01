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


