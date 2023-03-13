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
                 (* n (factorial-alt (- n 1))))))))
