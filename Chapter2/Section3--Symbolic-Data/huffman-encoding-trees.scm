;; 2.3.4 Huffman Encoding Trees

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

;; Ex. 2.67
#|
(A D A B B C A)
|#
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

;; Ex. 2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol x tree)
  (cond ((or (null? x) (null? tree)) '())
        ((element-of-set? x (symbols (left-branch tree)))
         (if (leaf? (left-branch tree))
             (list 0)
             ;; (append (list 0) (encode-symbol x (left-branch tree)))
             ;; or:
             (cons 0 (encode-symbol x (left-branch tree)))
             ))
        ((element-of-set? x (symbols (right-branch tree)))
         (if (leaf? (right-branch tree))
             (list 1)
             (cons 1 (encode-symbol x (right-branch tree)))))
        (else ;; not element of set
         (error "symbol not in tree" x))
        ))

(encode '(a d a b b c a) sample-tree)

;; Ex. 2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge set)
  (if (null? (cdr set))
      (car set)
      (successive-merge
       (adjoin-set (make-code-tree (car set) (cadr set)) (cddr set)))))

;; Ex. 2.70
#|
84 bits required

If fixed length code, one needs n = lg8 = 3 bits.

Line1: 3
Line2: 9
Line3: 3
Line4: 9
Line5: 10
Line6: 2

36 * 3 = 108 bits
|#

(define tree-2.70
  (generate-huffman-tree
   (list
    (list 'a 2)
    (list 'boom 1)
    (list 'get 2)
    (list 'job 2)
    (list 'na 16)
    (list 'sha 3)
    (list 'yip 9)
    (list 'wah 1))))

(define encoded-message
  (encode
   '(get a job
         sha na na na na na na na na
         get a job
         sha na na na na na na na na
         wah yip yip yip yip yip yip yip yip yip
         sha boom)
   tree-2.70))

(decode encoded-message tree-2.70)

;; Ex. 2.71
#|
See p. 36 for sketch.
most frequent encoded by 1 bit (1)
least frequent encoded by n-1 bits (0 0 ... 0)
|#

;; Ex. 2.72
#|
most frequent: (n - 1) + 1                                = n
next most frequent: (n - 1) + (n - 2) + 1                 = 2n - 2
next: (n - 1) + (n - 2) + (n - 3) + 1                     = 3n - 6 + 1
penultimate: (n - 1) + (n - 2) + (n - 3) + (n - 4) + 1    = 4n - 10 + 1
last: (n - 1) + (n - 2) + (n - 3) + (n - 4) + 1           = 4n - 10 + 1

1 + ∑ⱼ₌ₙ₋₁ⁱ j, j := n - position in ordered list     =>

In the best case, 𝒪(n)
In the worst case, 𝒪(n²)

To be rigorous, and since we are specifying relative frequency, we
can compute the expectation using p(i) = wᵢ / ∑ᵢ₌₁ⁿ wᵢ = 2ⁱ⁻¹ / ∑ᵢ₌₁ⁿ 2ⁱ⁻¹

𝔼[f(i)] = ∑ᵢ₌₁ⁿ f(i)p(i), f(i) = 1 + ∑ⱼ₌ᵢ₋₁ⁿ⁻¹
        = 1 + (1 / ∑ᵢ₌₂ⁿ 2ⁱ⁻¹) ∑ᵢ₌₁ⁿ (∑ⱼ₌ᵢ₋₁ⁿ⁻¹ j)2ⁱ⁻¹

Hence, 𝒪(n²), albeit, with a smaller constant than the worst case.
