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

;; Ex. 3.25
;; In a certain sense, the one-dimensional table satisfies the requirements,
;; as it already works with a list of keys, i.e. a list of keys is just a key.

(define (make-table) (list '*table*))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table (cons (cons key value) (cdr table)))))
  'ok)

;; Ex. 3.26
;; One can directly re-use the machinery of p. 155-161, with a few modifications.
;; The entry in the set must now represent the pair of key, value. Assuming that
;; we are dealing with either symbols or numbers, we can use the key as the source of
;; the ordering, in which case, the entry can be the pair itself, stored on
;; the first element of (list entry left right).
;; We need to update each use of =, >, < which acts on (entry set).
;; In fact, since these call sites follow a common pattern, it is simplest to provide
;; a separate implementation that acts on x := (key . value) and (entry set).
(define (key=? x y) (= (car x) (car y))) ;; We substitute key=?, key>?, key<?
(define (key>? x y) (> (car x) (car y))) ;; for =, >, <, respectively.
(define (key<? x y) (< (car x) (car y))) ;; element-of-set? is then has-key?
(define (has-key? key table) (element-of-set? (cons key '()) (cdr table)))
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((key=? x (entry set)) true)
        ((key<? x (entry set))
         (element-of-set? x (left-branch set)))
        ((key>? x (entry set))
         (element-of-set? x (right-branch set)))))

;; Insertion requires a mutating adjoin-set!
;; Also, we must have a non-empty tree to begin with.
(define (set-entry! tree x) (set-car! tree x))
(define (set-left-branch! tree x) (set-car! (cdr tree) x))
(define (set-right-branch! tree x) (set-car! (cddr tree) x))

(define (make-table) (cons '*table* (make-tree '() '() '())))

(define (adjoin-set! x set)
  (cond ((key=? x (entry set))
         (set-entry! set x))
        ((key<? x (entry set))
         (if (null? (left-branch set))
             (set-left-branch! set (make-tree x '() '()))
             (adjoin-set! x (left-branch set))))
        ((key>? x (entry set))
         (if (null? (right-branch set))
             (set-right-branch! set (make-tree x '() '()))
             (adjoin-set! x (right-branch set))))))

(define (insert! key value table)
  (if (null? (entry (cdr table)))
      (set-entry! (cdr table) (cons key value))
      (adjoin-set! (cons key value) (cdr table)))
  'ok)

(define (lookup key table)
  (cdr (assoc key (cdr table))))
(define (assoc key set)
  (cond ((null? set) false)
        ((= key (car (entry set))) (entry set))
        ((< key (car (entry set)))
         (assoc key (left-branch set)))
        ((> key (car (entry set)))
         (assoc key (right-branch set)))))

(define (make-tree entry left right)
  (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define tbl (make-table))
(insert! 1 'a tbl)
(insert! 2 'b tbl)
(insert! 3 'c tbl)
(insert! 100 'd tbl)
(insert! 25  'e tbl)

(lookup 3 tbl)

(has-key? 3 tbl)

;; Ex. 3.27
;; See p. 61-62 for the environment diagrams and associated notes.
;; The scheme will not work if we defined memo-fib to be (memoize fib)
;; as fib recursively calls itself. The table is searched only for the initial
;; value, but fib itself does not know of the table. Thus, upon failing to find
;; a result for the initial value, a regular call of fib is initiated.
;; If we want to realize savings on anything other than duplicated calls
;; (e.g. (memo-fib 3) (memo-fib 3)) we must make recursive functions
;; call their memoized version at each call-site.


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))
(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(memo-fib 100)

(define memo-fib-2 (memoize fib))
(memo-fib-2 30)
