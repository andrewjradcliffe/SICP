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

