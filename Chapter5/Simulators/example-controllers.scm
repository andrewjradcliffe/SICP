;; Useful controllers

(define recursive-factorial-registers
  '(continue n val))
(define recursive-factorial-operations
  (list (list '* *) (list '= =) (list '- -)))

(define recursive-factorial-controller-text
  '((assign continue (label fact-done))
    fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
    after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
    base-case
    (assign val (const 1))
    (goto (reg continue))
    fact-done))

(define gcd-registers
  '(a b t))
(define gcd-operations
  (list (list 'rem remainder) (list '= =)))

(define gcd-controller-text
  '(test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
    gcd-done))
