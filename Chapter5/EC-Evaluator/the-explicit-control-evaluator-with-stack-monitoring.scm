;; 5.4 The Explicit-Control Evaluator
#|
With stack monitoring.
|#

(load "~/aradclif/scheme-projects/SICP/Chapter5/EC-Evaluator/the-explicit-control-evaluator.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/EC-Evaluator/explicit-control-evaluator-controller-text-fragments.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            (set! current-depth (- current-depth 1))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))

(define print-result
  '(print-result
    (perform (op print-stack-statistics))
    (perform
     (op announce-output) (const ";;; EC-Eval value:"))
    (perform (op user-print) (reg val))
    (goto (label read-eval-print-loop))))

(define eceval-controller-text
  `(
    ,@read-eval-print-loop
    ,@print-result
    ,@unknown-expression-type
    ,@unknown-procedure-type
    ,@signal-error
    ,@eval-dispatch
    ,@ev-self-eval
    ,@ev-variable
    ,@ev-quoted
    ,@ev-lambda
    ,@ev-application
    ,@ev-appl-did-operator
    ,@ev-appl-operand-loop
    ,@ev-appl-accumulate-arg
    ,@ev-appl-last-arg
    ,@ev-appl-accum-last-arg
    ,@apply-dispatch
    ,@primitive-apply
    ,@compound-apply
    ,@ev-begin
    ,@ev-sequence
    ,@ev-sequence-continue
    ,@ev-sequence-last-exp
    ,@ev-if
    ,@ev-if-decide
    ,@ev-if-alternative
    ,@ev-if-consequent
    ,@ev-assignment
    ,@ev-assignment-1
    ,@ev-definition
    ,@ev-definition-1
    ))

(define eceval
  (make-machine
   eceval-registers
   eceval-operations
   eceval-controller-text))

;;;;;;;;;;;;;;;; Tests
(start eceval)
(define (factorial-rec n)
  (if (= n 1)
      1
      (* (factorial-rec (- n 1)) 1)))
