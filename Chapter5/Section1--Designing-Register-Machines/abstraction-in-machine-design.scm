;; 5.1.2 Abstraction in Machine Design

;; Ex. 5.3
#|
Full derivation, for posterity.
²
Begin from Taylor series expansion: ∑ᵢ₌₀ⁿ Dⁱf(x₀)(x - x₀)ⁱ / i!


f(x) = f(x₀) + Df(x₀)(x - x₀) + 1/2 D²f(x₀)(x - x₀)² + ℴ(x²)

Take derivative of quadratic approximation and set equal to zero (i.e. apply FONC)
Df(x) = 0 = Df(x₀) + D²f(x₀)(x - x₀)

x = x₀ - Df(x₀)/D²f(x₀)

or, for zero-finding:

x = x₀ - g(x₀)/Dg(x₀)

For square roots, we have y = x², i.e. find x such that x² = y <=> x = √y
Thus, g(x) = y - x²
     Dg(x) = -2x

x = x₀ - (y - x₀²)/(-2x₀) = 1/2 (y/x₀ + x₀)

If we happened to define g(x) = x² - y, then Dg(x) = 2x; we note that
(x² - y)/(2x) is equal to (y - x²)/(-2x), thus, we reach the same result.
|#

#|
Version 1: good-enough?, improve as primitives

See p. 188 for data paths and controller diagrams.
|#

(controller
 test-guess
 (test (op good-enough?) (reg guess))
 (branch (label sqrt-iter-done))
 (assign t-guess (op improve) (reg guess))
 (assign guess (reg t-guess))
 (goto (label test-guess))
 sqrt-iter-done)

#|
Version 2: expand good-enough?, improve into arithmetic operations

See p. 189-190 for data paths and controller diagrams.
|#

(controller
 test-good-enough?
 (assign s (op square) (reg guess))
 (assign m (op -) (reg s) (const x))
 (assign a (op abs) (reg m))
 (test (op <) (reg a) (const 0.001))
 (branch (label sqrt-iter-done))
 (assign q (op /) (const x) (reg guess))
 (assign t-guess (op average) (reg q) (reg guess))
 (assig guess (reg t-guess))
 (goto (label test-good-enough?))
 sqrt-iter-done)

;; We could achieve better clarity by inserting an entry point for improve,
;; though this creates an unnecessary instruction (i.e. (goto (label improve))).
;; On the other hand, it would be harmless to just have the improve entry point label.
(controller
 test-good-enough?
 (assign s (op square) (reg guess))
 (assign m (op -) (reg s) (const x))
 (assign a (op abs) (reg m))
 (test (op <) (reg a) (const 0.001))
 (branch (label sqrt-iter-done))
 (goto (label improve))
 improve
 (assign q (op /) (const x) (reg guess))
 (assign t-guess (op average) (reg q) (reg guess))
 (assig guess (reg t-guess))
 (goto (label test-good-enough?))
 sqrt-iter-done)
