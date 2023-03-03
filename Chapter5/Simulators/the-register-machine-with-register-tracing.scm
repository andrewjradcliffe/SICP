;; The vanilla register machine simulator, with additions from Section 5.2.4
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-vanilla-register-machine.scm")
(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/example-controllers.scm")

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

(define recursive-factorial-machine
  (make-machine
   '(continue n val)
   (list (list '* *) (list '= =) (list '- -))
   `(,@recursive-factorial-controller-text)))

(define (compute-recursive-factorial n)
  (set-register-contents! recursive-factorial-machine 'n n)
  (start recursive-factorial-machine)
  (get-register-contents recursive-factorial-machine 'val))

(trace-register recursive-factorial-machine 'val 'trace-on)
(compute-recursive-factorial 3)
(trace-register recursive-factorial-machine 'n 'trace-on)
(compute-recursive-factorial 3)
