;; 5.1.4 Using a Stack to Implement Recursion

;; Ex. 5.4
;; a
#|
See p. 191 for the data paths diagram.
|#
(controller
 (assign continue (label expt-done))
 expt-loop
 (test (op =) (reg n) (const 0))
 (branch (label base-case))
 (save continue)
 (assign n (op -) (reg n) (const 1))
 (assign continue (label after-expt))
 (goto (label expt-loop))
 after-expt
 (restore continue)
 (assign val (op *) (const b) (reg val))
 (goto (reg continue))
 base-case
 (assign val (const 1))
 (goto (reg continue))
 expt-done)


;; b
#|
See p. 192 for the data paths diagram.
|#
(controller
 (assign product (const 1))
 (assign counter (reg n))
 test-counter
 (test (op =) (reg counter) (const 0))
 (branch (label expt-iter-done))
 (assign product (op *) (const b) (reg product))
 (assign counter (op -) (reg counter) (const 1))
 (goto (label test-counter))
 expt-iter-done)


;; Ex. 5.5
#|
See p. 193-196 for the hand-simulation of (fact 3) and (fib 4)
|#

;; Ex. 5.6
#|
Within afterfib-n-1, the (restore continue) (save continue) can be removed.
|#
