;; 3.4.2 Mechanisms for Controlling Concurrency

;; Ex. 3.39
(parallel-execute
 (lambda () (set! x ((s (lambda () (* x x)))))) ;; access is serialized, but set! is not
 (s (lambda () (set! x (+ x 1)))))

;; Possibilities in which P2 has serial execution remain.
;; Possible results are
;;     101 : serial mul, set!, serial increment
;;     121 : serial increment, serialmul, set!
;;     100 : serial mul, serial increment, set!

