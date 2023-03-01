;; 5.2.3 Generating Execution Procedures for Instructions

;; Ex. 5.9
#|
A simple modification to make-operation-exp is sufficient.
|#
(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (if (label-exp? e)
                    (error "operation attempted on label: " exp)
                    (make-primitive-exp e machine labels)))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))


;; Ex. 5.10
#|
At first glance, a trivial new syntax -- however, if one were actually writing
code in the register-machine language, increment (and decrement) certainly occur
enough to make the saved keystrokes worthwhile. Note that an objection is
equivalent to rejecting the ++ operator of C/C++. Now, one admits, this is a lazy
syntax addition.

Form of new syntax:

(increment <reg-name>)


Below, I provide two versions, the first of which is a literal implementation,
and the latter as derived syntax (more general, though we would also need to require
the declaration of "one" for primitive + procedures other than real number arithmetic).

The latter uses the syntactic transformation:
(increment <reg-name>) ----> (assign <reg-name> (op +) (reg <reg-name>) (const 1))

Furthermore, the latter hooks into the downstream checking, and, as derived
syntax, does not add a primitive where one is not needed.
|#

(define (increment-reg-name increment-instruction)
  (cadr increment-instruction))

;; Version 1:
(define (make-increment inst machine pc)
  (let ((target
         (get-register machine (increment-reg-name inst))))
    (let ((value-proc
           (lambda ()
             (+ (get-contents target) 1))))
      (lambda ()
        (set-contents! target (value-proc))
        (advance-pc pc)))
    ;; Or, replace the second let block with:
    ;; (lambda ()
    ;;   (set-contents! target (+ (get-contents target) 1))
    ;;   (advance-pc pc))
    ))

;; To install, one adds the following within make-execution-procedure
((eq? (car inst) 'increment)
 (make-increment inst machine pc))


;; Version 2:
(define (make-increment inst machine labels operations pc)
  (let ((new-inst
         (list 'assign (increment-reg-name inst)
               (list 'op '+)
               (list 'reg (increment-reg-name inst))
               (list 'const 1))))
    (make-assign new-inst machine labels operations pc)))

;; To install, one adds the following within make-execution-procedure
((eq? (car inst) 'increment)
 (make-increment inst machine labels ops pc))


;; Ex. 5.11

;; a
#|
See p. 199 for an evaluation of the regular Fibonacci machine (after Ex. 5.6)
by hand, which demonstrates that removal of the following instruction is legal.

regular Fibonnaci machine, within afterfib-n-2 :
________________________________________________
(assign n (reg val))
(restore val)
(restore continue)
(assign val (op +) (reg val) (reg n))

revised Fibonnaci machine, within afterfib-n-2 :
________________________________________________
(restore n)
(restore continue)
(assign val (op +) (reg val) (reg n))

In the regular case, n is being used as "current-val" via
explicit assignment, after which "old-val" is restored into val.
Instead, val can be "current-val" and n can serve as the register into
which "old-val" is restored.
|#


;; b
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (cons (stack-inst-reg-name inst)
                        (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine sack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((popped (pop stack)))
        (if (eq? (car popped) (stack-inst-reg-name inst))
            (begin (set-contents! reg (cdr popped))
                   (advance-pc pc))
            (error "Bad RESTORE instruction -- mismatched registers"
                   (stack-inst-reg-name inst) (car popped)))))))

;; c
#|
There are several questions to consider:
- dynamic or static number of stacks?
- use current implementation or make larger changes?

Below, I implement two choices:
1. dynamic + current implementation
2. static + larger changes
|#

#|
Version 1.
Use extant stack to store (reg-name . stack-for-reg) elements, allocating
and pushing only as needed. Then, for restore, use assoc and pop off the top of the
cdr of the element (if it is found at all).
|#

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((reg-stack (assoc (stack-inst-reg-name inst) stack)))
        (if reg-stack
            (push (cdr reg-stack) (get-contents reg))
            (let ((new-stack (make-stack)))
              (push new-stack (get-contents reg))
              (push stack (cons (stack-inst-reg-name inst)
                                new-stack))))
        (advance-pc pc)))))


(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (let ((reg-stack (assoc (stack-inst-reg-name inst) stack)))
        (if reg-stack
            (begin (set-contents! reg (pop (cdr reg-stack)))
                   (advance-pc pc))
            (error "Bad RESTORE instruction -- stack not found for register: "
                   (stack-inst-reg-name inst)))))))


#|
Version 2.
Since the number of possible stacks is equal to the number of registers,
and, furthermore, is known at the time of machine creation, one can modify
the machine creation procedure. This allows for far more efficient execution
procedures, as each stack can be looked up at assembly-time.
|#

(define (make-machine register-names op controller-text)
  (let ((machine (make-new-machine register-names)))
    (for-each (lambda (register-name)
                ((machine 'allocte-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))


(define (make-new-machine register-names)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (map (lambda (name) (cons name (make-stack))) register-names))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda ()
                         (for-each (lambda (s) (s 'initialize)) stack)))))
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
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst)))
        (reg-stack (cdr (assoc (stack-inst-reg-name inst) stack))))
    (lambda ()
      (push reg-stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst)))
        (reg-stack (cdr (assoc (stack-inst-reg-name inst) stack))))
    (lambda ()
      (set-contents! reg (pop reg-stack))
      (advance-pc pc))))


;; Ex. 5.12
#|
It is preferable to use sets (based on binary trees) to implement
these operations, but these naive implementations should work.

These procedures incur superfluous passes over the instructions.
Notably, this could be done in a single pass; furthermore,
constructing each quantity incrementally using binary tree-backed sets
would confer 𝒪(logn) time-complexity.

As written below, unique is 𝒪(n^2) and the map and filter are each 𝒪(n).

We have 4 total lists to construct, using the following procedures:
- unique-sorted-instructions                    𝒪(n^2)
- entry-point-registers                         𝒪(n^2)
- save-or-restore-registers                     𝒪(n^2)
- assign-sources (used by next)                 𝒪(n^2)
- registers-and-assign-sources                  𝒪(n^3)

Notably, we need only pay the 𝒪(n^2) + time-complexity of sort
(presumably, no worse than 𝒪(nlogn)) cost a single time,
as one can construct the rest from the result of unique-sorted-instructions.
Still, them middle 3 each have 𝒪(n^2) + 𝒪(n) + 𝒪(n) = 𝒪(n^2) time-complexity -- not cheap!

registers-and-assign-sources actually has 𝒪(n^3) complexity -- we take the complexity
of the assign-sources to be 𝒪(n^2), and, as plainly seen, we perform this operation
a number of times equal to the number of unique registers -- we might be kind to ourselves
and acknowledge that there are only m <= n unique registers in an attempt to justify
writing 𝒪(m*n^2), but, as we know, n/k = m, hence, 𝒪((n/k) * n^2) = 𝒪(n^3).
One might want to think of m << n, but that's best-case thinking -- better to plan for the
worst-case (and average-case!).
|#


(define (unique sequence)
  (if (null? sequence)
      '()
      (let ((first (car sequence))
            (rest (cdr sequence)))
        (if (member first rest)
            (unique rest)
            (cons first
                  (unique rest))))))
;; Use built-in sort instead
;; (define (partial-sort sequence op)
;;   (cond ((null? sequence) '())
;;         ((null? (cdr sequence)) sequence)
;;         (else
;;          (let ((first (car sequence))
;;                (second (cadr sequence))
;;                (rest (cddr sequence)))
;;            (if (op first second)
;;                (cons first
;;                      (partial-sort (cons second rest) op))
;;                (cons second
;;                      (partial-sort (cons first rest) op)))))))
;; (define (my-sort sequence op)
;;   (let ((maybe (partial-sort sequence op)))
;;     (if (equal? maybe sequence)
;;         maybe
;;         (sort maybe op))))
;; (my-sort '(3 3 1 2 3 4 1 3 2 1 1) <)

;; In essence, we analyze the instruction sequence and extract relevant information.

;; list of all instructions, with duplicates removed, sorted by instruction type
(define (unique-sorted-instructions insts)
  (unique (sort insts (lambda (x y) (symbol<? (car x) (car y))))))
;; (unique-sorted-instructions '((a 1) (c 3) (b 2) (b 2)))

;; list (without duplicates) of the registers used to hold entry points
(define (goto? inst) (tagged-list? inst 'goto))
(define (goto-register? inst)
  (and (goto? inst) (register-exp? (cadr inst))))
(define (goto-inst-reg-name inst) (register-exp-reg (cadr inst)))
(define (entry-point-registers insts)
  (unique (map got-inst-reg-name (filter goto-register? insts))))

;; list (without duplicates) of the registers that are saved or restored
(define (save? inst) (tagged-list? inst 'save))
(define (restore? inst) (tagged-list? inst 'restore))
(define (save-or-restore? inst)
  (or (save? inst) (restore? inst)))
(define (save-or-restore-registers insts)
  (unique (map stack-inst-reg-name (filter save-or-restore? insts))))

;; for each register, list (without duplicates) of the sources from which it is assigned
(define (assign? inst) (tagged-list? inst 'assign))
(define (assign-sources reg-name assign-insts)
  (unique (map assign-value-exp
               (filter (lambda (x)
                         (eq? (assign-reg-name x) reg-name))
                       assign-insts))))
(define (registers-and-assign-sources insts)
  (let ((assign-insts (filter assign? insts)))
    (let ((reg-names (unique (map assign-reg-name assign-insts))))
      (map (lambda (reg-name)
             (cons reg-name (assign-sources reg-name assign-insts)))
           reg-names))))

;; Updated make-new-machine, machine-machine
(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (unique-instructions '())
        (entry-point-regs '())
        (save-or-restore-regs '())
        (regs-and-assign-sources '()))
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
              ;; new dispatches
              ((eq? message 'install-new-information)
               (set! unique-instructions
                     (unique-sorted-instructions
                      (map instruction-text the-instruction-sequence)))
               (set! entry-point-regs
                     (entry-point-registers unique-instructions))
               (set! save-or-restore-regs
                     (save-or-restore-registers unique-instructions))
               (set! regs-and-assign-sources
                     (registers-and-assign-sources unique-instructions)))
              ((eq? message 'unique-instructions) unique-instructions)
              ((eq? message 'entry-point-registers) entry-point-regs)
              ((eq? message 'save-or-restore-registers) save-or-restore-regs)
              ((eq? message 'registers-and-assign-sources) regs-and-assign-sources)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    (machine 'install-new-information)
    machine))



;; Ex. 5.13
#|
There are a few ways to enact this. The simplest way to do this is to scan out
the registers from instructions -- this can be done during assembly, once the labels
have been extracted. I denote this as Version 1.

Another option is to dynamically perform the allocation within
make-assign, make-primitive-exp, make-goto, make-save, and make-restore --
wherever the register first happens to be used.
From the perspective of make-assign, we must delay the target and potential allocation
until after the instructions have been generated for value-exp,
as make-primitive-exp (which is called within make-operation-exp) may encounter
register expressions which necessitate allocation; the register(s) allocated
within make-primitive-exp may actually be the same register which is being assigned to.

The general solution is to create a new procedure which either returns the
previously-allocated register, or allocates a new one, which is then returned.
We then replace all occurrences of get-register with get-or-allocate-register within
make-assign, make-primitive-exp, make-goto, make-save, and make-restore.
I denote this as Version 2.

In both versions, we modify make-machine as shown below.
|#
;; modified make-machine

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; Version 1
(define (update-register-table! insts machine)
  (let ((unique-insts (unique (map instruction-text insts))))
    (let ((reg-names (unique (map assign-reg-name
                                  (filter assign? unique-insts)))))
      (for-each (lambda (register-name)
                  ((machine 'allocate-register) register-name))
                (reg-names)))))

(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-register-table! insts machine)
                    (update-insts! insts labels machine)
                    insts)))

;; Version 2
(define (get-or-allocate-register machine register-name)
  (let ((reg (get-register machine register-name)))
    (if reg
        reg
        (begin ((machine 'allocate-register) register-name)
               (get-register machine register-name)))))
