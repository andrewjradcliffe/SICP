;; Compiler with lexical addressing
#|
Strictly for test of lexical addressing -- excludes additions from Ex. 5.38
|#

;; Under normal circumstances, one would load just the compiler, but we also need a
;; machine simulator. As the machine primitives differ from the compiled-code-evaluator
;; without open-coded primitives, we just re-define things here.
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-register-machine-with-stack-monitoring.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Compiler/vanilla-compiler.scm")


;; modified compile
(define (compile exp target linkage compile-time-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp)
         (compile-variable exp target linkage compile-time-env))
        ((assignment? exp)
         (compile-assigment exp target linkage compile-time-env))
        ((definition? exp)
         (compile-definition exp target linkage compile-time-env))
        ((if? exp)
         (compile-if exp target linkage compile-time-env))
        ((lambda? exp)
         (compile-lambda exp target linkage compile-time-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
                           compile-time-env))
        ((application? exp)
         (compile-application exp target linkage compile-time-env))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (compile-definition exp target linkage compile-time-env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next compile-time-env)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence '(env val) (list target)
                                            `((perform (op define-variable!)
                                                       (const ,var)
                                                       (reg val)
                                                       (reg env))
                                              (assign ,target (const ok))))))))

(define (compile-if exp target linkage compile-time-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next compile-time-env))
            (c-code
             (compile
              (if-consequent exp) target consequent-linkage compile-time-env))
            (a-code
             (compile
              (if-alternative exp) target linkage compile-time-env)))
        (preserving '(env continue)
                    p-code
                    (append-instruction-sequences
                     (make-instruction-sequence '(val) '()
                                                `((test (op false?) (reg val))
                                                  (branch (label ,f-branch))))
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     after-if))))))

(define (compile-sequence seq target linkage compile-time-env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage compile-time-env)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next compile-time-env)
                  (compile-sequence (rest-exps seq) target linkage compile-time-env))))

(define (compile-application exp target linkage compile-time-env)
  (let ((proc-code (compile (operator exp) 'proc 'next compile-time-env))
        (operand-codes
         (map (lambda (operand) (compile operand 'val 'next compile-time-env))
              (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage)))))


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
                          `((perform op (lexical-address-set!)
                                     (const ,lexical-address)
                                     (reg val)
                                     (reg env))
                            (assign ,target (const ok))))))))))



;; Ex. 5.43
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


(define (compile-lambda exp target linkage compile-time-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage
         lambda-linkage
         (make-instruction-sequence '(env) (list target)
                                    `((assign ,target
                                              (op make-compiled-procedure)
                                              (label ,proc-entry)
                                              (reg env)))))
        (compile-lambda-body
         (lambda-internal-transform exp) proc-entry compile-time-env))
       after-lambda))))


;; in addition to the standard machine primitives, we now need to expose the
;; primitives we plan to open-code. Due to the nature of the global-environment,
;; we need to re-start the machine each time. In essence, define the procedure and
;; splice it into the list.
(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)
(define compiled-code-operations
  (list
   ;; machine primitives from Scheme
   (list 'cons cons)
   (list 'list list)
   (list '+ +)
   (list '- -)
   (list '* *)
   (list '= =)
   ;; truth
   (list 'true? true?)
   (list 'false? false?)
   ;; environment operations
   (list 'lexical-address-lookup lexical-address-lookup)
   (list 'lexical-address-set! lexical-address-set!)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
   (list 'extend-environment extend-environment)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   ;; needs to be spliced in when machine is created.
   ;; (list 'get-global-environment get-global-environment)
   ))
