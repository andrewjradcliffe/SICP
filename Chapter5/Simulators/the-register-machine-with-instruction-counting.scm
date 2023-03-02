;; The vanilla register machine simulator, with additions from Section 5.2.4
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-vanilla-register-machine.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/example-controllers.scm")


(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 (list 'print-instrunction-count
                       (lambda () (print-instrunction-count)))
                 (list 'reset-instruction-count
                       (lambda () (reset-instruction-count!)))))
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
      (define (increment-instruction-count!)
        (set! instruction-count (+ instruction-count 1)))
      (define (reset-instruction-count!)
        (set! instruction-count 0))
      (define (print-instruction-count)
        (newline)
        (display list 'instruction-count '= instruction-count))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (increment-instruction-count!)
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
              ((eq? message 'increment-instruction-count)
               (increment-instruction-count!))
              ((eq? message 'reset-instruction-count)
               (reset-instruction-count))
              ((eq? message 'print-instruction-count)
               (print-instruction-count))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))


;;;;;;;;;;;;;;;; Test
(define recursive-factorial-machine
  (make-machine
   '(continue n val)
   (list (list '* *) (list '= =) (list '- -))
   `((perform (op reset-instruction-count))
     ,@recursive-factorial-controller-text
     (perform (op print-instruction-count)))))

(define (compute-recursive-factorial n)
  (set-register-contents! recursive-factorial-machine 'n n)
  (start recursive-factorial-machine)
  (get-register-contents recursive-factorial-machine 'val))

(define (recursive-factorial-interactive)
  (let ((n (read)))
    (newline)
    (display "(factorial ")
    (display n)
    (display ")")
    (let ((val (compute-recursive-factorial n)))
      (newline)
      (display "val = ")
      (display val)
      (newline)
      (newline)))
  (recursive-factorial-interactive))