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
  (let (e (make-wire))
    (and-gate a b e)
    (inverter e output))
  'ok)

(define (xor-gate a b output)
  (let ((c (make-wire)) (d (make-wire)))
    (or-gate a b c)
    (nand-gate a b d)
    (and-gate c d output))
  'ok)


;; Ex 3.30

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
;; then the SUM and CARRY delay of a full-adder are equal.
;; In such a case, the total time is n * (2 and-gate-delay + 2 or-gate-delay)


(define (ripple-carry-adder A B S C)
  (cond ((and (= (length A) (length B)) (= (length A) (length S)))
         (define (iter A B C-in S C-out)
           (cond ((null? (cdr A))
                  (full-adder (car A) (car B) C-in (car S) C-out))
                 (else
                  (let ((new-C (make-wire)))
                    (full-adder (car A) (car B) C-in (car S) new-C)
                    (iter (cdr A) (cdr B) new-C (cdr S) C-out)))))
         (iter (reverse A) (reverse B) (make-wire) (reverse S) C)
         'ok)
        (else
         (error "Unequal lengths" (list A B S)))))


