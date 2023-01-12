;; 3.3.3 Representing Tables

;; Ex. 3.24

(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (assoc key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table (cons (cons key value) (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown method -- MAKE-TABLE"))))
    dispatch))

;; External procedures
(define (lookup key table) ((table 'lookup) key))
(define (insert! key value table) ((table 'insert!) key value))

(define tbl (make-table equal?))
(lookup 'this tbl)
(insert! 'a 1 tbl)
(insert! 'b 2 tbl)
(insert! 'c 3 tbl)
(lookup 'a tbl)
(lookup 'b tbl)
(lookup 'c tbl)
(insert! '(1 2 3) 4 tbl)
(lookup '(1 2 3) tbl)
