;; 4.1.6 Internal Definitions

;; Ex. 4.16

;; a
;; not using the abstractions from Ex. 4.12
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                 (error "Unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; b

;; Version 1.1: in forward order, the long way
(define (scan-out-defines exps)
  (define (partition defines regulars exps)
    (if (null? exps)
        (cons defines regulars)
        (let ((first (car exps))
              (rest (cdr exps)))
          (if (definition? first)
              (partition (append defines (list first)) regulars rest)
              (partition defines (append regulars (list first)) rest)))))
  (let ((defines-regulars (partition '() '() exps)))
    (let ((defines (car defines-regulars))
          (regulars (cdr defines-regulars)))
      (if (null? defines)
          exps ;; or, regulars, which is equivalent
          (let ((vars (map definition-variable defines))
                (vals (map definition-value defines)))
            (make-let (map (lambda (x) (list x '*unassigned*)) vars)
                      (append (map (lambda (x y) (list 'set! x y)) vars vals)
                              regulars)))))))


;; Version 1.2: in forward order, the short way
(define (scan-out-defines exps)
  (let ((defines (filter definition? exps))
        (regulars (filter (lambda (x) (not (definition? x))) exps)))
    (if (null? defines)
        exps ;; or, regulars, which is equivalent
        (let ((vars (map definition-variable defines))
              (vals (map definition-value defines)))
          (make-let (map (lambda (x) (list x '*unassigned*)) vars)
                    (append (map (lambda (x y) (list 'set! x y)) vars vals)
                            regulars))))))

;; Version 2: in reverse order, the long way
(define (scan-out-defines exps)
  (define (partition defines regulars exps)
    ;; Note cons: creates defines, regulars in reverse
    (if (null? exps)
        (cons defines regulars)
        (let ((first (car exps))
              (rest (cdr exps)))
          (if (definition? first)
              (partition (cons first defines) regulars rest)
              (partition defines (cons first regulars) rest)))))
  (define (iter defines bindings exps)
    (if (null? defines)
        (make-let bindings exps)
        (let ((var (definition-variable (car defines)))
              (val (definition-value (car defines))))
          (iter (cdr defines)
                (cons (list var '*unassigned*) bindings)
                (cons (list 'set! var val) exps)))))
  (let ((defines-regulars (partition '() '() exps)))
    (let ((defines (car defines-regulars))
          (regulars (cdr defines-regulars)))
      (if (null? defines)
          exps ;; or, regulars, which is equivalent
          (iter defines '() (reverse regulars))))))

;; Clearly, Version 1.2 achieves the best clarity of intent, though,
;; it is the least efficient. Admittedly, since we are going to use analysis,
;; efficiency does not make much of a difference. Besides -- if we really
;; wanted to optimize, we could construct everything in one pass, but it would
;; be quite a mess.


;; Version 3: everything together. As it happens, it is actually not quite as messy
;; as anticipated.
(define (scan-out-defines exps)
  (define (iter bindings set!-exps body-exps exps)
    (if (null? exps)
        (if (null? bindings)
            body-exps
            (make-let bindings (append set!-exps body-exps)))
        (let ((first (car exps))
              (rest (cdr exps)))
          (if (definition? first)
              (let ((var (definition-variable first))
                    (val (definition-value first)))
                (iter (append bindings (list var '*unassigned*))
                      (append set!-exps (list 'set! var val))
                      body-exps
                      rest))
              (iter bindings set!-exps (append body-exps (list first)) rest)))))
  (iter '() '() '() exps))


;; c

;; If in make-procedure:
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

;; If in procedure-body:
(define (procedure-body p) (scan-out-defines (caddr p)))

;; From an efficiency perspective, placing this in make-procedure is preferable,
;; as the defines are scanned out a single time -- when the procedure is created.
;; On the other hand, if placed in procedure-body, each time the procedure is
;; applied, the defines will be scanned out. Clearly, the latter is far less efficient.

