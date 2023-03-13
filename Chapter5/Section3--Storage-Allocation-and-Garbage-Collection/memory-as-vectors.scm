;; 5.3.1 Memory as Vectors

;; Ex. 5.20
#|
See p. 213 for the drawing.

The final value of free is p4.
p1 represents x
p3 represents y
|#

;; Ex. 5.21

;; a
(controller
 (assign continue (label loop-done))
 (assign val (const 0))
 loop
 (test (op null?) (reg tree))
 (branch (label null-case))
 (test (op pair?) (reg tree))
 (branch (label pair-case))
 (assign n (const 1))
 (goto (reg continue))
 null-case
 (assign n (const 0))
 (goto (reg continue))
 pair-case
 (save continue)
 (save tree)
 (assign continue (label right))
 (assign tree (op car) (reg tree))
 (goto (label loop))
 right
 (assign val (op +) (reg n) (reg val))
 (restore tree)
 (assign tree (op car) (reg tree))
 (restore continue)
 (goto (label loop))
 loop-done
 (assign val (op +) (reg n) (reg val)))

;; b
(controller
 (assign continue (label loop-done))
 (assign val (const 0))
 loop
 (test (op null?) (reg tree))
 (branch (label base-case))
 (test (op pair?) (reg tree))
 (branch (label pair-case))
 (assign val (op +) (reg val) (const 1))
 (goto (reg continue))
 pair-case
 (save continue)
 (save tree)
 (assign continue (label right))
 (assign tree (op car) (reg tree))
 (goto (label loop))
 right
 (restore tree)
 (assign tree (op cdr) (reg tree))
 (restore continue)
 (goto (label loop))
 base-case
 (goto (reg continue))
 loop-done)


;; Ex. 5.22

#|
In essence, for each pair, store the first element on the stack, preceded by the
continue label. This builds up a stack of alternating values and labels, with
values in the reverse order in which they are encountered on x, e.g.
(2 cons-loop 1 loop-done)

Then, one proceeds to process the stack, with the next action directed by the restored
continue register.
|#
;; append
(controller
 (assign continue (label loop-done))
 loop
 (test (op pair?) (reg x))
 (branch (label cons-case))
 (goto (reg continue))
 cons-case
 (assign z (op car) (reg x))
 (assign x (op cdr) (reg x))
 (save continue)
 (save z)
 (assign continue (label cons-loop))
 (goto (label loop))
 cons-loop
 (restore z)
 (restore continue)
 (assign y (op cons) (reg z) (reg y))
 (goto (reg continue))
 loop-done)

#|
In essence, save the pointer to x in the register z so that we may restore
x after the operation is complete, then cdr down x, saving a pointer to the
last pair in register w. Set the cdr of w to y, then restore x from z.
|#
;; append!
(controller
 (assign continue (label loop-done))
 (assign z (reg x))
 loop
 (test (op pair?) (reg x))
 (branch (label advance))
 (goto (reg continue))
 advance
 (assign w (reg x))
 (assign x (op cdr) (reg x))
 (goto (label loop))
 loop-done
 (perform (op set-cdr!) (reg w) (reg y))
 (assign x (reg z)))
