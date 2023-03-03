;; The vanilla register machine simulator, with additions from Section 5.2.4
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-vanilla-register-machine.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/example-controllers.scm")

;; Ex. 5.16 and 5.17, combined.

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (trace false))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))))
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
      (define (execute-with-trace)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                (let ((text (instruction-text (car insts)))
                      (label (instruction-label (car insts))))
                  (if (not (null? label))
                      (begin
                        (newline)
                        (display label)))
                  (newline)
                  (display text))
                ((instruction-execution-proc (car insts)))
                (execute-with-trace)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (if trace
                   (execute-with-trace)
                   (execute)))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ((eq? message 'trace-on)
               (set! trace true))
              ((eq? message 'trace-off)
               (set! trace false))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))
(define (instruction-text inst) (caar inst))
(define (instruction-label inst) (cadar inst))
(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (begin
                                (if (not (null? insts))
                                    (let ((first (car insts)))
                                      (set-car! (cdar first) next-inst)))
                                (receive insts
                                    (cons (make-label-entry next-inst
                                                            insts)
                                          labels)))
                              (receive (cons (make-instruction
                                              (list next-inst '()))
                                             insts)
                                  labels)))))))

;;;;;;;;;;;;;;;; Test
(define recursive-factorial-machine
  (make-machine
   '(continue n val)
   (list (list '* *) (list '= =) (list '- -))
   `(,@recursive-factorial-controller-text)))

(define (compute-recursive-factorial n)
  (set-register-contents! recursive-factorial-machine 'n n)
  (start recursive-factorial-machine)
  (get-register-contents recursive-factorial-machine 'val))

(recursive-factorial-machine 'trace-on)
(compute-recursive-factorial 3)
(recursive-factorial-machine 'trace-off)
(compute-recursive-factorial 3)
(recursive-factorial-machine 'trace-on)

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

(recursive-factorial-interactive)

;; (define x (extract-labels recursive-factorial-controller-text
;;                           (lambda (insts labels)
;;                             (update-insts! insts labels recursive-factorial-machine)
;;                             insts)))
;; (define x0 (cadr x))
;; (instruction-text x0)
;; (instruction-label x0)
