;; 2.4.3 Data-Directed Progamming and Additivity

;; Ex. 2.73

;; a
;; The clauses starting with (sum? ...) (product? ...) (exponentation? ...)
;; are moved into a method table for deriv then looked up with (get 'deriv op)
;; where op is extracted via operator.
;; The predicates cannot be assimilated as they do not have an operator which
;; could be looked up in the method table -- they also have no operands,
;; hence the else clause would need heavy revision. The difference in arity (1, (1, 2), 3)
;; clauses reflects the different requirements. One could, with much unnecessary
;; complication, move the logic into operator/operands, but not only would this
;; be highly convoluted, it would obfuscate the simple nature of the predicates.

;; b
(define (install-deriv-sum)
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-deriv-product)
  (define (deriv-product exp var)
    (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier exp) var) (multiplicand exp))))
  (put 'deriv '* deriv-product)
  'done)

;; c
(define (install-deriv-exponentation)
  (define (deriv-exponentation exp var)
    (make-product (exponent exp)
                  (make-product (make-exponentation (base exp) (- (exponent exp) 1))
                                (deriv (base exp) var))))
  'done)

;; d
;; This requires that the corresponding derivative be installed into the respective
;; operator table, e.g. 'deriv-sum into '+ table via (put '+ 'deriv deriv-sum).
;; Otherwise, little change is required.

;; Ex. 2.74

;; a
(define (get-record file name)
  ((get 'get-record (division file)) (file-contents file) name))
;; The file must be tagged with a division type-tag. To simplify things, let us
;; make it the first element in a list, and the file itself the rest of the list structure.
(define (division x) (car x))
(define (file-contents x) (cdr x))

;; b
(define (get-salary record)
  ((get 'get-salary (record-type record)) (record-contents record)))
(define (record-type x) (car x))
(define (record-contents x) (cdr x))
;; If the record is tagged with a type-tag which identifies the record type, then one can
;; dispatch on record alone. Let's suppose that we place it as the first element in a list.
;; This leads to a 2-dimensional table on record type, and also encourages re-use of record
;; types across divisions. On the other hand, if each division requires their own version
;; of record-type, then would require
(define (get-salary record-type record)
  ((get 'get-salary (record-type record)) (record-contents record)))
;; and get-salary suddenly needs to have external context simply to get the record type.
;; It's trivial to cons on a type to any record type, thus, do not both with
;; convoluted designs.

;; c
;; assume that get-record returns '() if it fails to find the requested record
(define (find-employee-record files name)
  (if (null? files)
      '()
      (let ((record (get-record (car files) name)))
        (if (not (null? record))
            record
            (find-employee-record (cdr files) name)))))

;; d
;; Assuming that none of Insatiable's existing divisions are merging with
;; the newly-acquired divisions, for each new division, create a file of names,
;; type-tagging where appropriate to dispatch onto a potentially new type of record
;; (tagged appropriately), the method for which would also need to be created.
;; This may be as simple as adding type-tags to division files and records.

;; Ex. 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; Ex. 2.76

;; Explicit dispatch:
;; Each new type necessitates an additional clause in each generic operation --
;; the code must be modified literally at each generic definition
;; and a non-colliding name chosen for the operation respective to type.

;; Data-directed style:
;; (op dispatches on type(s))
;; Each new type must add a column to the method table to enable dispatching
;; (for the methods that are sensible -- one need not fill all rows for each type).
;; Each new operation adds a row to the table, defined at will for 1 or more types.
;; In terms of code, each type and its methods are separate, sand the dispatching
;; is all handled through a common interface.

;; Message-passing style:
;; (type dispatches on op)
;; Each new type necessitates a new procedural constructor in which its dispatch will be defined
;; (a new column in the method table).
;; Each new operation necessitates that a new clause be added to the procedural constructor.

;; Most appropriate for frequent addition of new types:
;; Data-directed style as the generic operations then encompass a larger variety
;; of combinations of types.

;; Most appropriate for frequent addition of new operations:
;; Message-passing style, as this constitutes a scenario in which one wishes to perform many
;; operations on few data types (but 1-arg limitation is still a problem).

;; op dispatch on type(s): easy to extend to new types, no limit on number of arguments
;;                         in generic procedure

;; type dispatch on op: easy to extend to new ops, but limit of 1 argument in generic procedure

;; To be a useful op, message-passing simply needs to define the op on a given type.
;; To be a useful op with data-directed style, one is encouraged to define it for all types;
;; but if one already has an op and adds a new type, it is little effort to extend.
;; Ultimately, depends on how one intends to pick and choose.

;; e.g.
;;                     type
;;             real          complex
;;        ________________________________
;; o    + |             |                |
;; p    - |             |                |
;; e    * |             |                |
;; r    / |             |                |
;; a
;; t
;; i
;; o
;; n
