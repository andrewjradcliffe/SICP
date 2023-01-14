;; 3.3.4 A Simulator for Digital Circuits

;; ⊕ ∧ ∨ ¬ ⊻ ⊼ ⊽
;; exclusive-or:    ⊕    (a ∨ b) ∧ ¬(a ∧ b)
;; set-difference:  \    a ∧ (a ⊕ b)
;;                       a ∧ ¬(a ∧ b)    simplifies to this as a = a ∧ (a ∨ b)
;; nand:            ⊼    ¬(a ∧ b)
;;                       ¬a ∨ ¬b
;; nor:             ⊽    ¬a ∧ ¬b
;;                       ¬(a ∨ b)

;; Ex. 3.28

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)

;; These logical ops could exit early, but all inputs are checked
;; in order to detect the presence of any invalid signals (and error if found).
(define (logical-or a b)
  (cond ((= a 0)
         (cond ((= b 1) 1)
               ((= b 0) 0)
               (else (error "Invalid signal" b))))
        ((= a 1) 1)
        (else (error "Invalid signal" a))))

(define (logical-and a b)
  (cond ((= a 1)
         (cond ((= b 1) 1)
               ((= b 0) 0)
               (else (error "Invalid signal" b))))
        ((= a 0)
         (cond ((= b 1) 0)
               ((= b 0) 0)
               (else (error "Invalid signal" b))))
        (else (error "Invalid signal" a))))

(define (logical-not a)
  (cond ((= a 1) 0)
        ((= a 0) 1)
        (else (error "Invalid signal" a))))

;; Ex. 3.29

;; OR truth table
;;
;;     a    b    F(a,b)
;;     0    0    0
;;     0    1    1
;;     1    0    1
;;     1    1    1
;;
;; This can be expressed as ¬(¬a ∧ ¬b)
(define (nor-gate a b output)
  (let ((c (make-wire)) (d (make-wire)))
    (inverter a c)
    (inverter b d)
    (and-gate c d output))
  'ok)

(define (or-gate a b output)
  (let ((e (make-wire)))
    (nor-gate a b e)
    (inverter e output))
  'ok)

;; Other useful gates
(define (nand-gate a b output)
  (let ((e (make-wire)))
    (and-gate a b e)
    (inverter e output))
  'ok)

(define (xor-gate a b output)
  (let ((c (make-wire)) (d (make-wire)))
    (or-gate a b c)
    (nand-gate a b d)
    (and-gate c d output))
  'ok)


;; Ex. 3.30

;; half-adder:     2 and-gate
;;                 1 or-gate
;;                 1 inverter
;; full-adder:     2 half-adder    =    4 and-gate
;;                 1 or-gate            3 or-gate
;;                                      2 inverter
;; ripple-carry:   n full-adder    =    4n and-gate
;; -adder                               3n or-gate
;;                                      2n or-gate
;; Sequential as carry must be available before any steps in each full-adder
;; can occur. In a full-adder, the or-gate to which A is sent is technically
;; free of prior delays, but it requires the signal from the half-adder
;; to which B is sent in order to compute its value. Hence, one must wait for the
;; lower half-adder before proceeding with the upper half-adder.
;; In essence, the delay is equal to the longest path through the circuit.
;; ---
;; Time spent in half-adder depends on delay of components.
;; If or-gate-delay > and-gate-delay, then the inverter can operate in parallel
;; with the remainder of the or-gate-delay. The final and-gate waits on both
;; branches, hence, the delay can be expressed as:
;; SUM delay:
;;                 1 and-gate-delay
;;                 + max {(1 and-gate-delay + 1 inverter-delay), 1 or-gate-delay}
;; CARRY delay:
;;                 1 and-gate-delay
;; ---
;; Time spent in the full-adder is:
;; SUM delay:
;;                 SUM delay of the bottom half-adder
;;                 + SUM delay of the top half-adder
;;
;;                 = 2 and-gate-delay
;;                 + 2 max {(1 and-gate-delay + 1 inverter-delay), 1 or-gate-delay}
;; CARRY delay:
;;                 SUM delay of the bottom half-adder
;;                 + CARRY delay of top half-adder
;;                 + 1 or-gate-delay
;;
;;                 = 2 and-gate-delay
;;                 + max {(1 and-gate-delay + 1 inverter-delay), 1 or-gate-delay}
;;                 + 1 or-gate-delay
;; ---
;; Time spent in ripple-carry-adder for n-bit binary numbers
;; SUM delay:
;;                 n * SUM delay of full-adder
;;
;; CARRY delay:
;;                 n * CARRY delay of full-adder
;;
;; However, the sequential nature of the ripple-carry-adder means that while
;; the A and B can be loaded onto the full-adder, nothing can proceed until
;; the carry is available. Thus, one waits at least as long as n * CARRY delay of full-adder.
;; If the or-gate-delay is greater than (1 and-gate-delay + 1 inverter-delay),
;; then the SUM and CARRY delay of a full-adder are equal. As Ex. 3.29 demonstrates,
;; an or-gate always has higher delay than 1 and-gate + 1 inverter.
;; Therefore, the total time is n * (2 and-gate-delay + 2 or-gate-delay)

(define (ripple-carry-adder A B S C)
  (define (iter A B C-in S C-out)
    (cond ((null? (cdr A))
           (full-adder (car A) (car B) C-in (car S) C-out))
          (else
           (let ((new-C (make-wire)))
             (full-adder (car A) (car B) C-in (car S) new-C)
             (iter (cdr A) (cdr B) new-C (cdr S) C-out)))))
  (cond ((and (= (length A) (length B)) (= (length A) (length S)))
         (iter (reverse A) (reverse B) (make-wire) (reverse S) C)
         'ok)
        (else
         (error "Unequal lengths" (list A B S)))))


;; Ex. 3.31, Ex 3.32
;; See p. 64 of notes.

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)
(set-signal! input-2 1)
(propagate)

(define the-agenda (make-agenda))
(define a-1 (make-wire))
(define a-2 (make-wire))
(define c-1 (make-wire))
(define s-1 (make-wire))
(define c-2 (make-wire))

(probe 'a-1 a-1)
(probe 'a-2 a-2)
(probe 'c-1 c-1)
(probe 's-1 s-1)
(probe 'c-2 c-2)

(full-adder a-1 a-2 c-1 s-1 c-2)

(set-signal! a-1 1)
(set-signal! a-2 1)
(propagate)

(current-time the-agenda)

;;;;;;;;;;;;;;;;
;; Other procedures necessary to complete working simulator

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s))
  'ok)

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out))
  'ok)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; Representing wires
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))


(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

;; The agenda

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
