;; 5.2

;; Ex. 5.7

(define recursive-expt-machine
  (make-machine
   '(n val b continue)
   (list
    (list '* *)
    (list '= =)
    (list '- -))
   '((assign continue (label expt-done))
     expt-loop
     (test (op =) (reg n) (const 0))
     (branch (label base-case))
     (save continue)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-expt))
     (goto (label expt-loop))
     after-expt
     (restore continue)
     (assign val (op *) (reg b) (reg val))
     (goto (reg continue))
     base-case
     (assign val (const 1))
     (goto (reg continue))
     expt-done)))
(set-register-contents! recursive-expt-machine 'n 2)
(set-register-contents! recursive-expt-machine 'b 3)
(start recursive-expt-machine)
(get-register-contents recursive-expt-machine 'val)



(define iterative-expt-machine
  (make-machine
   '(product counter b n)
   (list
    (list '* *)
    (list '= =)
    (list '- -))
   '((assign product (const 1))
     (assign counter (reg n))
     test-counter
     (test (op =) (reg counter) (const 0))
     (branch (label expt-iter-done))
     (assign product (op *) (reg b) (reg product))
     (assign counter (op -) (reg counter) (const 1))
     (goto (label test-counter))
     expt-iter-done)))
(set-register-contents! iterative-expt-machine 'n 2)
(set-register-contents! iterative-expt-machine 'b 3)
(start iterative-expt-machine)
(get-register-contents iterative-expt-machine 'val)
