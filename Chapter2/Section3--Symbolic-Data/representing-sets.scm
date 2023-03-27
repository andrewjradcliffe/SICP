;; 2.3.3 Representing Sets

;; Ex. 2.59
#|
This delays the growth of set2 such that in the worst case,
the size of set2 does not change during the element-of-set? calls.
The size only changes once the cons sequence begins, which takes place
only after all elements in set2 have been exhausted.
|#
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))

;; Ex. 2.60
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (union-set set1 set2) (append set1 set2))
(define (intersection-set set1 set2)
  (cond ((or (null? set) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

#|
Efficiency:
adjoin and union are 𝒪(1) and 𝒪(n), respectively.
element-of-set? is still 𝒪(n), but n is likely larger (potentially much larger)
intersection-set is still 𝒪(n²), but n is likely larger (potentially much larger)

Preference:
If one had an application that primarily involved adjoin and union operations only,
this would be preferable. However, it makes element-of-set? and intersection-set
potentially very expensive.
|#

;; Ex. 2.61
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; Ex. 2.62
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (let ((x1 (car set1))
              (x2 (car set2)))
          (cond ((= x1 x2)
                 (cons x1 (union-set (cdr set1) (cdr set2))))
                ((< x1 x2)
                 (cons x1 (union-set (cdr set1) set2)))
                ((> x1 x2)
                 (cons x2 (union-set set1 (cdr set2))))))))

;; Ex. 2.63

;; a
#|
same for Figure 2.16 left: (1 3 5 7 9 11)
2.16 middle: (1 3 5 7 9 11); tree->list-1
2.16 right:  (1 3 5 7 9 11); tree->list-2
|#

;; b
#|
tree->list-1 performs more work due to append requiring n cons operations.

f(n) = | 0                if n = 0
       | 1                if n = 1
       | 2f((n-1)/2) + 1 + (n-1)/2           if n = 3, 7, 15, ...


This is 𝒪(nlgn), but tree->list-2 is 𝒪(n)
|#

;; Ex. 2.64

#|

a.

partial-tree recursively halves the ordered list, creating a left half
which has length of 1 less than the right half, and reserving the 1 element
as the entry of the tree to be formed at the end of the call.
partial-tree then calls itself on the left and right halves, proceeding
recursively until a size of 1 is encountered, at which point a tree comprised of the
reserved element (within that call) and two empty lists is formed.
This tree and the remaining elements are returned to the caller, which will in
turn form a tree comprised of this tree and the tree formed from the remaining elements.

Construction proceeds from the left, with elements being depleted from the left
half before the right half. (if complete balance cannot be achieved)

Thus, while in the ideal case the sequence of halving would proceed until
left/right results are produced at size=1, it is possible to arrive at a case
where the left half is of size 0 and the right half is of size 1. While it may appear
arbitrary as to which half is assigned size 0, it is in fact necessary to deplete
the left first in order to maintain order in the binary tree.

Progress through the ordered list is tracked by returning from each call
a pair of the tree and the remaining elements. Each call to partial-tree car's off
1 element, after which subsequent calls act only on the cdr of elts.


b.

2n + 1 calls to partial-tree, hence, 𝒪(n)
|#


;; Ex. 2.65
#|
list->tree                𝒪(2n)
union-set (ordered set)   𝒪(n)
tree->list-2              𝒪(n)
tree->list-2              𝒪(n)

𝒪(5n) = 𝒪(n)
|#
(define (union-set-tree tree1 tree2)
  (list->tree (union-set (tree->list-2 tree1) (tree->list-2 list2))))
(define (intersection-set-tree tree1 tree2)
  (list->tree (intersection-set (tree->list-2 tree1) (tree->list-2 list2))))


;; Ex. 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        ((> given-key (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))))
