;; 5.2.4 Monitoring Machine Performance

;; Ex. 5.14

(define recursive-factorial-machine
  (make-machine
   '(continue n val)
   (list (list '* *) (list '= =) (list '- -))
   `((perform (op initialize-stack))
     ,@recursive-factorial-controller-text
     (perform (op print-stack-statistics)))))

(define (compute-recursive-factorial n)
  (set-register-contents! recursive-factorial-machine 'n n)
  (start recursive-factorial-machine)
  (get-register-contents recursive-factorial-machine 'val))

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


;; (define (factorial n)
;;   (define (iter product counter)
;;     (if (> counter n)
;;         product
;;         (iter (* counter product) (+ counter 1))))
;;   (iter 1 1))

#|
In fact, initialize-stack is not automatically called anywhere.
However, we can use perform instructions to include it in the controller.

n total-pushes maximum-depth
2 2            2
3 4            4
4 6            6
.
.
.

2(n-1) = 2n - 2
|#


;; Ex. 5.17
#|
Two versions, the first of which is very indirect, inefficient, and not extensible.
The second version is direct, efficient and extensible (as demonstrated in Ex. 5.19).
|#

#|
Version 1:
Store the labels in the simulator, and during execution with trace on,
compare equal? the cdr of labels elements to the current insts -- if a match
is found, print the label.
|#
(define (match-insts insts labels)
  (if (null? labels)
      false
      (let ((label (car labels)))
        (if (equal? insts (cdr label))
            (car label)
            (match-insts insts cdr labels)))))
;; Within execute with trace on:
(let ((label (match-insts insts labels)))
  (if label
      (begin (newline)
             (display label))))
;; Within assemble's lambda
(update-labels! machine labels)
#|
However, this is horribly inefficient -- one could perform the matching a single
time, after the update-insts!, by modifying the car of each instruction.
|#
(define (add-labels! insts labels)
  (let ((label (match-insts insts label))
        (inst (car insts)))
    (set-car! inst (cons (car inst) label))
    (add-labels! (cdr insts) labels)))
;; Within execute with trace on:
(if trace
    (let ((text-label (instruction-text (car insts))))
      (if (cdr text-label)
          (begin
            (newline)
            (display (cdr text-label))))
      (newline)
      (display (car text-label))))


#|
Version 2:
Create the pairs of either (text . label) or (text . (label . ())) in extract-labels.
The former is sufficient for Ex. 5.17, but the latter is preferable as it enables
Ex. 5.19.
Then, all that needs to be done is update the definitions of instruction-text
and instruction-label.
|#
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
                                      (set-cdr! (cdar first) next-inst)))
                                (receive insts
                                    (cons (make-label-entry next-inst
                                                            insts)
                                          labels)))
                              (receive (cons (make-instruction
                                              (list next-inst '()))
                                             insts)
                                  labels)))))))
;; Within execute:
(if trace
    (let ((text (instruction-text (car insts)))
          (label (instruction-label (car insts))))
      (if (not (null? label))
          (begin
            (newline)
            (display label)))
      (newline)
      (display text)))

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
#|
One can build upon the labeled instructions introduced in Ex. 5.17, as they
allow one to identify label positions relative to instructions.
It is preferable to utilize the program counter mechanism as-is, insofar
as stop/proceed are concerned.

The approach: modify the label-part of the instruction to signify
that it constitutes a breakpoint.

The convention is that the label-part of an instruction contains
information which immediately precedes the instruction.
One can then have both a label and breakpoint prior to the first instruction
in a block of instructions.
We shall consider the "offset" to be <n>, which can be the offset from
the label as it appears in the controller text; N.B. this is the
only truly sensible way to report offset. This is consistent with
the offset from a list which begins at the label in the controller sequence.
Since we store the label on the subsequent (w.r.t. where the label appears in the
controller-text) instruction, we subtract 1 when using list-ref.
Importantly, this enables us to handle n = 1 seamlessly.

Given the concept of blocks of instructions demarcated by labels above and
(perhaps) below, additional rigor can be achieved for breakpoint creation/removal.
Consider that unless we assert that for a given label and offset the proposed
breakpoint must be within the block belonging to said label, it is possible to set
(or cancel) a breakpoint within a different block; this is a direct result of
the linear order given by the controller sequence.
One should enforce that the breakpoint belong to the same block as the label.

We must make a brief detour to define valid locations for breakpoints
within a block. It seems to be nonsense to consider a breakpoint placed
after the last instruction in a block -- this would be equivalent to a placing
a breakpoint at the start of the next block, which, if desired, should be explicitly
specified by providing the label and offset (1) for the next block.
If allowed, this would make it possible to set a breakpoint with one label and offset,
then cancel it with another -- nonsense! insofar as a debugger is concerned.

As an example, consider the gcd-machine:

controller-text                                     instruction index      controller-text index
_______________                                     _________________      _____________________

'(test-b                                                                          0
  (test (op =) (reg b) (const 0))                          0                      1
  (branch (label gcd-done))                                1                      2
  (assign t (op rem) (reg a) (reg b))                      2                      3
  (assign a (reg b))                                       3                      4
  (assign b (reg t))                                       4                      5
  (goto (label test-b))                                    5                      6
  gcd-done                                                                        7
  (perform (op print-stack-statistics)))                   6                      8

After extracting the labels, we have a list of length 7. There are two blocks,
which we shall refer to as test-b and gcd-done, respectively.

(set-breakpoint gcd-machine 'test-b <n>) is valid for n=1,...,6.
That is, for offsets which belong to the same block as the label, reading from the
controller-text (offsets into the list are adjusted by -1, but we hide this from the
user so that printed offsets match input offsets).

The test-b block is a sub-list of length 6. Valid breakpoints within this block
correspond to adjusted offsets of 0,...,5. Once we have separated the blocks,
into sub-lists, we can satisfy both requirements (creation/removal always corresponds to
breakpoint in block, no breakpoints after last instruction in block) with a single
predicate: (< (- n 1) (length sub-list))
Which is just the warmly familiar: the offset must provide a valid index into the sub-list.

In the implementation, I do not explicitly create the sub-lists;
instead, the distance between beginning of blocks is used.
|#

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

;; Internal methods, to be placed within make-new-machine
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
;; Then, add the dispatches to make-new-machine
((eq? message 'proceed)
 (execute))

((eq? message 'set-breakpoint) set-breakpoint)
((eq? message 'cancel-breakpoint) cancel-breakpoint)
((eq? message 'cancel-all-breakpoints)
 (cancel-all-breakpoints))

;; External methods
(define (proceed-machine machine)
  (machine 'proceed))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))
(define (cancel-breakpoint machine label n)
  ((machine 'cancel-breakpoint) label n))
(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define (instruction-label-part) (cdar inst))

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
    (null? (cdr label-part))))

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
