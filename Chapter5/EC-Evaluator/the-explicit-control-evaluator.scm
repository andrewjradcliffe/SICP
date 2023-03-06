;; 5.4 The Explicit-Control Evaluator
#|
Drawing upon a several sources, hence, a separate directory.
|#

(load "~/aradclif/scheme-projects/SICP/Chapter5/Simulators/the-vanilla-register-machine.scm")

(define eceval-operations
  (list
   ;; machine primitives from Scheme
   (list 'read read)
   ;; machine primitives from 4.1
   (list 'self-evaluating? self-evaluating?)
        ))
