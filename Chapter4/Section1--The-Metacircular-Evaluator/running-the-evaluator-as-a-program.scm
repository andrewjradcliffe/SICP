;; 4.1.4 Running the Evaluator as a Program

;; Ex. 4.14
#|
When entered into the metacircular evaluator, this adds a procedure to the
metacircular evaluator's environment. As it is defined in terms of the
metacircular evaluator's language, the special forms, derived expressions
and primitive operations are all evaluated on the implemented language.

On the other hand, Louis' installation of the map primitive into the
implemented language using the implementation language's map will result in
the implementation language operating on the representation of the procedures
and expressions belong to the implemented language. Given that our procedures
are represented as lists, the implementation language's map will choke on the
first attempt at procedure application.

We would end up with a call to apply that looks like:
(apply map args)
Where args is a list of expressions, the first of which will be taken
to be the proc and the rest either directing to the vararg version of map,
or, if only two args, then the list to which proc will be applied.
The former will assuredly fail catastrophically, and the latter will also
fail, but perhaps less catastrophically.
|#
