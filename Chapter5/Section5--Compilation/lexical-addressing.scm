;; 5.5.6 Lexical Addressing

(define (make-lexical-address f-num d-num) (list f-num d-num))
(define (frame-number lexical-address) (car lexical-address))
(define (displacement-number lexical-address) (cadr lexical-address))

;; Ex. 5.39
(define (lexical-address-lookup lexical-address runtime-env)
  (let ((f-num (frame-number lexical-address))
        (d-num (displacement-number lexical-address)))
    (let ((frame (list-ref runtime-env f-num)))
      (let ((val (list-ref (frame-values frame) d-num)))
        (if (eq? val '*unassigned*)
            (error "Unassigned variable"
                   (list-ref (frame-variables frame) d-num))
            val)))))

(define (lexical-address-set! lexical-address value runtime-env)
  (let ((f-num (frame-number lexical-address))
        (d-num (displacement-number lexical-address)))
    (let ((frame (list-ref runtime-env f-num)))
      (if (eq? (list-ref-set-car! (frame-values frame) d-num value) 'not-ok)
          (error "Bad lexical address" lexical-address runtime-env)
          'ok))))
(define (list-ref-set-car! seq n value)
  (if (null? seq)
      'not-ok
      (if (= n 0)
          (set-car! seq value)
          (list-ref-set-car! (cdr seq) (- n 1) value))))

;; Ex. 5.40
(define empty-compile-time-environment '())
(define (extend-compile-time-environment formals compile-time-env)
  (cons formals compile-time-env))
(define (first-compile-time-frame compile-time-env) (car compile-time-env))
(define (enclosing-compile-time-environment compile-time-env) (cdr compile-time-env))

;; to every procedure that performs compilation, add another argument: compile-time-env
;; these changes are made in compiler-with-lexical-addressing.scm

(define (compile-lambda-body exp proc-entry compile-time-env)
  (let ((formals (lambda-parameters exp)))
    (let ((extended-env
           (extend-compile-time-environment formals compile-time-env)))
      (append-instruction-sequences
       (make-instruction-sequence '(env proc argl) '(env)
                                  `(,proc-entry
                                    (assign env (op compiled-procedure-env) (reg proc))
                                    (assign env
                                            (op extend-environment)
                                            (const ,formals)
                                            (reg argl)
                                            (reg env))))
       (compile-sequence (lambda-body exp) 'val 'return extended-env)))))



;; Ex. 5.41
(define (find-variable var compile-time-env)
  (define (env-loop frame-number env)
    (if (null? env)
        'not-found
        (let ((frame (first-compile-time-frame env)))
          (let ((displacement-number (scan 0 frame)))
            (if (eq? displacement-number 'not-found)
                (env-loop (+ frame-number 1)
                          (enclosing-compile-time-environment env))
                (make-lexical-address
                 frame-number
                 displacement-number))))))
  (define (scan displacement-number variables)
    (if (null? variables)
        'not-found
        (if (eq? var (car variables))
            displacement-number
            (scan (+ displacement-number 1) (cdr variables)))))
  (env-loop 0 compile-time-env))


;; Ex. 5.42
(define (compile-variable exp target linkage compile-time-env)
  (let ((lexical-address (find-variable exp compile-time-env)))
    (if (eq? 'not-found lexical-address)
        (end-with-linkage
         linkage
         (make-instruction-sequence '(env) (list target)
                                    ;; A daring way to avoid a save/restore of env;
                                    ;; otherwise: save env, assign the global-env to env
                                    ;; assign target, restore env.
                                    ;; Anything stored in target is about to be clobbered
                                    ;; anyway, thus, it does not matter if we temporarily
                                    ;; set it to something other than the final value.
                                    `((assign ,target (op get-global-environment))
                                      (assign ,target
                                              (op lookup-variable-value)
                                              (const ,exp)
                                              (reg ,target)))))
        (end-with-linkage
         linkage
         (make-instruction-sequence '(env) (list target)
                                    `((assign ,target
                                              (op lexical-address-lookup)
                                              (const ,lexical-address)
                                              (reg env))))))))


(define (compile-assigment exp target linkage compile-time-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next compile-time-env)))
    (let ((lexical-address (find-variable var compile-time-env)))
      (if (eq? 'not-found lexical-address)
          (end-with-linkage
           linkage
           (preserving '(env)
                        get-value-code
                        (make-instruction-sequence
                         '(env val) (list target)
                          ;; Here, however, the analogy to above is potentially fatal
                          ;; as the value being set! is stored in val, and if
                          ;; target is val, then we destroy the value.
                          ;; Save/restore of env seems inevitable.
                          `((save env)
                            (assign env (op get-global-environment))
                            (perform (op set-variable-value!)
                                     (const ,var)
                                     (reg val)
                                     (reg env))
                            (restore env)
                            (assign ,target (const ok))))))
          (end-with-linkage
           linkage
           (preserving '(env)
                        get-value-code
                        (make-instruction-sequence
                         '(env val) (list target)
                          `((perform (op lexical-address-set!)
                                     (const ,lexical-address)
                                     (reg val)
                                     (reg env))
                            (assign ,target (const ok))))))))))


;; Ex. 5.43
#|
A procedure body is identical to a lambda body, thus, if we perform the syntactic
transformation on the lambda body, then we have successfully scanned out internal
definitions -- should should be possible to accomplish by changing the last line
of compile-lambda-body to be:
(compile-sequence (internal-definitions-transform (lambda-body exp)) 'val 'return extended-env)

Notably, the scan-out-defines of Ex. 4.16 only transforms to a let statement, which we do not
quite support yet. Thus, it is worthwhile to implement the transformation directly to lambda.


Upon second thought, it is perhaps preferable to perform the transformation within
compile-lambda instead of compile-lambda-body. Thus, we change the penultimate line in
compile-lambda to be:
(compile-lambda-body (lambda-internal-transform exp) proc-entry compile-time-env)
|#
(define (unassigned-list n)
  (if (= n 0)
      '()
      (cons ''*unassigned* (unassigned-list (- n 1)))))

(define (internal-definitions-transform body-exps)
  (let ((defines (filter definition? body-exps)))
    (if (null? defines)
        body-exps
        (let ((regulars (filter (lambda (exp) (not (definition? exp))) body-exps))
              (variables (map definition-variable defines))
              (value-exps (map definition-value defines)))
          (let ((set!-exps (map (lambda (var exp)
                                  (list 'set! var exp))
                                variables
                                value-exps)))
            (list ;; makes this a body with single expression; necessary for body to be sequence
             (cons ;; make this an application
              (make-lambda variables
                           (append set!-exps regulars))
              (unassigned-list (length defines)))))))))

(define (lambda-internal-transform exp)
  (make-lambda (lambda-parameters exp)
               (internal-definitions-transform (lambda-body exp))))


;; Ex. 5.44
#|
There are two approaches:
1. modify compile-open-code, which is not only more involved, but also potentially implies
   modifications elsewhere.
2. more nuanced predicates and dispatch within compile

The latter is not only more elegant, it is more flexible.
|#

(define (application-open-code? exp compile-time-env)
  (if (pair? exp)
      (let ((op (operator exp)))
        (and (or (eq? op '+) (eq? op '-) (eq? op '*) (eq? op '=))
             (eq? 'not-found (find-variable op compile-time-env))))
      false))

(define (application-open-code-varargs? exp compile-time-env)
  (if (pair? exp)
      (let ((op (operator exp)))
        (and (or (eq? op '+) (eq? op '*))
             (not (= (length (operands exp)) 2))
             (eq? 'not-found (find-variable op compile-time-env))))
      false))

