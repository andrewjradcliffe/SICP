;; 5.5.5 An Example of Compiled Code

(define (print-instruction instruction)
  (newline)
  (if (symbol? instruction)
      (display instruction)
      (begin (display "  ")
             (display instruction))))
(define (print-compiled-instruction-sequence seq)
  (for-each print-instruction (statements seq)))

;; Ex. 5.33
;; Figure 5.17's code
(define figure-5.17
  (begin (reset-label-counter)
         (compile
          '(define (factorial n)
             (if (= n 1)
                 1
                 (* (factorial (- n 1)) n)))
          'val
          'next)))
(print-compiled-instruction-sequence figure-5.17)

(define factorial-alt
  (begin (reset-label-counter)
         (compile
          '(define (factorial-alt n)
             (if (= n 1)
                 1
                 (* n (factorial-alt (- n 1)))))
          'val
          'next)))
(print-compiled-instruction-sequence factorial-alt)

;; Performance tests?
(define factorial-3
  (begin (reset-label-counter)
         (compile
          '(begin
             (define (factorial n)
               (if (= n 1)
                   1
                   (* (factorial (- n 1)) n)))
             (factorial 3))
          'val
          'next)))
(print-compiled-instruction-sequence factorial-3)

(define (compiled-factorial-eval-with-monitoring n)
  (define compiled-machine
    (make-machine
     all-regs
     compiled-code-operations
     `(
       (perform (op initialize-stack))
       ,@(statements
          (begin (reset-label-counter)
                 (compile
                  `(begin
                     (define (factorial n)
                       (if (= n 1)
                           1
                           (* (factorial (- n 1)) n)))
                     (factorial ,n))
                  'val
                  'next)
                 ))
       (perform (op print-stack-statistics))
       )
     ))
  (define the-global-environment (setup-environment))
  (define (get-global-environment) the-global-environment)
  (set-register-contents! compiled-machine 'env (get-global-environment))
  (start compiled-machine)
  (get-register-contents compiled-machine 'val)
  )

(compiled-factorial-eval-with-monitoring 6)
