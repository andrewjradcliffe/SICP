;; The vanilla register machine simulator, with all additions from Section 5.2.4
#|
Given that Ex. 5.19 depends on Ex. 5.17, why not just add everything together, since
the others are orthogonal components.
|#

(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-vanilla-register-machine.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/example-controllers.scm")

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (instruction-count 0)
        (trace false))
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
      ;; Ex. 5.15
      (define (increment-instruction-count!)
        (set! instruction-count (+ instruction-count 1)))
      (define (reset-instruction-count!)
        (set! instruction-count 0))
      (define (print-instruction-count)
        (newline)
        (display (list 'instruction-count '= instruction-count)))
      ;; Ex. 5.19
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let ((inst (car insts)))
                (if (breakpoint? inst)
                    (if (active-breakpoint? inst)
                        (begin
                          (print-breakpoint inst)
                          (deactivate-breakpoint! inst)
                          'broke)
                        (begin
                          ;; this branch is entered upon reactivation of the machine after break
                          (activate-breakpoint! inst)
                          ((instruction-execution-proc inst))
                          (execute)))
                    (begin
                      ((instruction-execution-proc inst))
                      (execute)))))))
      ;; Ex. 5.16 and 5.17, updated to Ex. 5.19
      (define (execute-with-trace)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (let ((inst (car insts)))
                (if (breakpoint? inst)
                    (if (active-breakpoint? inst)
                        (begin
                          (print-breakpoint inst)
                          (deactivate-breakpoint! inst)
                          'broke)
                        (begin
                          ;; this branch is entered upon reactivation of the machine after break
                          (activate-breakpoint! inst)
                          (print-trace inst)
                          ((instruction-execution-proc inst))
                          (execute-with-trace)))
                    (begin
                      (print-trace inst)
                      ((instruction-execution-proc inst))
                      (execute-with-trace)))))))
      ;; Ex. 5.19
      (define (set-breakpoint label n)
        (let ((insts
               (lookup-instructions-label the-instruction-sequence label)))
          (if insts
              (if (offset-within-block? insts n)
                  (let ((inst (list-ref insts (- n 1))))
                    (create-breakpoint! inst label n))
                  (error "offset outside block belonging to label -- SET-BREAKPOINT"
                         (list label n)))
              (error "label not found -- SET-BREAKPOINT" label))))

      (define (cancel-breakpoint label n)
        (let ((insts
               (lookup-instructions-label the-instruction-sequence label)))
          (if insts
              (if (offset-within-block? insts n)
                  (let ((inst (list-ref insts (- n 1))))
                    (remove-breakpoint! inst label n))
                  (error "offset outside block belonging to label -- CANCEL-BREAKPOINT"
                         (list label n)))
              (error "label not found -- CANCEL-BREAKPOINT" label))))

      (define (cancel-all-breakpoints)
        (for-each remove-breakpoint! the-instruction-sequence))
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
              ;; Ex. 5.15
              ((eq? message 'increment-instruction-count)
               (increment-instruction-count!))
              ((eq? message 'reset-instruction-count)
               (reset-instruction-count!))
              ((eq? message 'print-instruction-count)
               (print-instruction-count))
              ;; Ex. 5.16 and 5.17
              ((eq? message 'trace-on)
               (set! trace true))
              ((eq? message 'trace-off)
               (set! trace false))
              ;; Ex. 5.19
              ((eq? message 'proceed)
               (if trace
                   (execute-with-trace)
                   (execute)))
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'cancel-breakpoint) cancel-breakpoint)
              ((eq? message 'cancel-all-breakpoints)
               (cancel-all-breakpoints))
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; Ex. 5.14
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


;; Ex. 5.15
(define (print-instruction-count machine)
  (machine 'print-instruction-count))
(define (reset-instruction-count machine)
  (machine 'reset-instruction-count))


;; Ex. 5.17
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

;; Ex. 5.18
(define (make-register name)
  (let ((contents '*unassigned*)
        (trace false))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (if trace
                   (begin
                     (newline)
                     (display "register ")
                     (display name)
                     (newline)
                     (display "old contents ")
                     (display contents)
                     (newline)
                     (display "new contents ")
                     (display value)))
               (set! contents value)))
            ((eq? message 'trace-on)
             (set! trace true))
            ((eq? message 'trace-off)
             (set! trace false))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (trace-register machine reg-name on-off)
  (if (or (eq? on-off 'trace-on) (eq? on-off 'trace-off))
      ((get-register machine reg-name) on-off)
      (error "Bad register trace message" on-off)))

;; Ex. 5.19
(define (distance-to-next-label insts)  ;; assuming that he first inst does not have a label
  (define (iter count seq)
    (if (null? seq)
        count
        (if (not (null? (instruction-label (car seq))))
            count
            (iter (+ count 1) (cdr seq)))))
  (iter 0 insts))
(define (offset-within-block? insts n) ;; assumes that first inst has a label
  (< (- n 1) (+ (distance-to-next-label (cdr insts)) 1)))

(define (lookup-instructions-label insts label-name)
  (if (null? insts)
      false
      (if (eq? (instruction-label (car insts)) label-name)
          insts
          (lookup-instructions-label (cdr insts) label-name))))
(define (proceed-machine machine)
  (machine 'proceed))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))
(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (instruction-label-part inst) (cdar inst))

(define (print-breakpoint inst)
  (let ((label-part (instruction-label-part inst)))
    (let ((label (cadr label-part))
          (offset (caddr label-part)))
      (newline)
      (display "breakpoint: ")
      (display (list '(label offset) '= (list label offset))))))

(define (create-breakpoint! inst label offset)
  (let ((label-part (instruction-label-part inst)))
    (if (null? (cdr label-part))
        (set-cdr! label-part (list label offset true)))))
(define (remove-breakpoint! inst)
  (let ((label-part (instruction-label-part inst)))
    (set-cdr! label-part '())))

(define (breakpoint? inst)
  (let ((label-part (instruction-label-part inst)))
    (not (null? (cdr label-part)))))

(define (active-breakpoint? inst)
  (let ((label-part (instruction-label-part inst)))
    (if (null? (cdr label-part))
        false
        (cadddr label-part))))

(define (activate-breakpoint! inst)
  (let ((label-part (instruction-label-part inst)))
    (if (null? (cdr label-part))
        (error "instruction does not contain breakpoint -- ACTIVATE-BREAKPOINT!" inst)
        (set-car! (cdddr label-part) true))))

(define (deactivate-breakpoint! inst)
  (let ((label-part (instruction-label-part inst)))
    (if (null? (cdr label-part))
        (error "instruction does not contain breakpoint -- DEACTIVATE-BREAKPOINT!" inst)
        (set-car! (cdddr label-part) false))))

;; For use with instruction tracing -- reduces repetition
(define (print-trace inst)
  (let ((text (instruction-text inst))
        (label (instruction-label inst)))
    (if (not (null? label))
        (begin
          (newline)
          (display label)))
    (newline)
    (display text)))


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


(define gcd-machine
  (make-machine
   gcd-registers
   gcd-operations
   gcd-controller-text))

(gcd-machine 'trace-on)
(trace-register gcd-machine 'a 'trace-on)
(trace-register gcd-machine 'b 'trace-on)
(trace-register gcd-machine 't 'trace-on)
(set-breakpoint gcd-machine 'test-b 4)
(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(get-register-contents gcd-machine 'a)
(get-register-contents gcd-machine 'b)
(get-register-contents gcd-machine 't)
(start gcd-machine)
(proceed-machine gcd-machine)
