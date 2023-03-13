;; 5.1.1 A Language for Describing Register Machines

;; Ex. 5.2

;; long-form
(data-paths
 (registers
  ((name product)
   (buttons ((name product<-t-product) (source (register t-product)))))
  ((name counter)
   (buttons ((name counter<-t-counter) (source (register t-counter)))))
  ((name t-product)
   (buttons ((name t-product<-m) (source (operation *)))))
  ((name t-counter)
   (buttons ((name t-counter<-a) (source (operation +))))))
 (opeations
  ((name *)
   (inputs (register product) (register counter)))
  ((name +)
   (inputs (register counter) (constant 1)))
  ((name >)
   (inputs (register counter) (register n)))))
(controller
 test-counter
 (test >)
 (branch (label iter-done))
 (t-product<-m)
 (t-counter<-a)
 (product<-t-product)
 (counter<-t-counter)
 (goto (label test-counter))
 iter-done)

;; short-form
(controller
 test-counter
 (test (op >) (reg counter) (reg n))
 (branch (label iter-done))
 (assign t-product (op *) (reg counter) (reg-product))
 (assign t-counter (op +) (reg counter) (const 1))
 (assign product (reg t-product))
 (assign counter (reg t-counter))
 (goto (label test-counter))
 iter-done)

#|
Technically, t-product and t-counter can be eliminated as long as the assignment
to product occurs first, i.e.
(assign product (op *) (reg counter) (reg product))
(assign counter (op +) (reg counter) (const 1))
|#
