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

;; Ex. 3.40
;; See p. 76-78 for illustration and values
(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (* x x x))))

;; Ex. 3.41
(define (print-balance account)
  (newline)
  (display (get-balance account)))

(define acc (make-account 100))
(define (get-balance account) (account 'balance))

(parallel-execute
 (lambda () (withdraw acc 25))
 (lambda () (print-balance acc)))

#|
The above parallel-execute could print either 75 or 100, depending on order of
execution. If the value returned by get-balance were relevant to some
computation, then unserialized access to the balance appears to be problematic.
If it were serialized, then at least one would have a guarantee that a withdraw
or deposit was not in progress when get-balance returns a value.
Otherwise, we could access balance at any point in a withdraw/deposit,
potentially at an indeterminate state. Though, one whether would want to access
balance when one knows that concurrent withdraw/deposits are occurring is dubious;
nonetheless, it seems preferable to err on the side of caution.
|#

;; Ex. 3.42
#|
This depends on the nature of the serializer. If the statement from p. 304 (SICP)
is read literally, then the proposed change creates a set of 2 procedures,
and parallel exeuction of a collection would be appropriately protected
as the statement implies that it is the set itself which is locked during
execution. In other words, during either protected-withdraw or protected-deposit,
calls arising elsewhere are not able to proceed -- this necessitates locking
of both the set and the procedure-being-executed?

(parallel-execute (lambda () (withdraw acc 25))
                  (lambda () (withdraw acc 10)))

On the other hand, if only the set of procedures is locked, but the individual
procedures are unlocked, then parallel-executes would be problematic.
Actually, if the set is locked, then this would technically be enough to make
the change safe. It could work as follows:
call of a serialized procedure on some args:
1) creates a (lambda () (procedure args))
2) adds the generated procedure to a queue
3) checks if locked; if not, take first item from queue and run it, otherwise
   sleep a while then check the lock again.


See bottom of p. 84 for additional information.
|#

;; Ex. 3.43
#|
See p. 79-82 for illustration, detailed commentary and conclusions.
|#

;; Ex. 3.44
#|
Louis is wrong. The essential difference between the transfer problem
and the exchange problem is that the exchange problem involves a parameter (difference)
which is computed from the respective values of balance in account1, account2
at a time prior to the initiation of either serial procedure.
The value computed is current only for the balances at the time of access, and the
balance may in fact change between the computation of difference and the initiation
of withdraw and/or deposit.
(In fact, any number of changes to balance, for either account, may be interleaved
between the computation of difference and initiation of withdraw and/or deposit).
This form of dependence is one in which the correct behavior is dependent on
two states remaining the same from point of access through any subsequent actions
which are based on the derived value. (we might set! two states based on a single
value derived from both).

On the other hand, transfer involves a parameter (amount) which is independent
of the respective states for either account. Abstracted, the transfer is just a bundled
call to withdraw/deposit on two accounts given some external value.
|#

;; Ex. 3.45
#|
The error in reasoning arises from the failure to consider what would occur if
a problem which itself is serialized attempts to use the protected procedure
returned by (account 'withdraw) or (account 'deposit).
When the serializer of the procedure which calls either of the two is the same
as the serializer which protects the procedures returned, a call to the protected
procedure will be forced to wait until the serialized procedure currently being
executed has completed -- but completion can never occur, as that requires that the
protected procedure be executed to completion.

In essence, one or more serializers protect a block of code. Inside the block of code,
a procedure protected by one or more or the serializers which protect the code block
is to be called. When called, this protected procedure will be forced to wait
indefinitely, as the serializer which protects it is already engaged by the block
of code in which the call appears. In other words, in order to execute, the serializer
must be freed, but the serializer cannot be freed until the block of code completes.
|#

;; Ex. 3.46
#|
See p. 84 for illustration.

Both processes think they have acquired the mutex as the return value is false,
hence, no loop is initiation.
The alignment of the operation need not be exactly as laid out above -- all that is
required is tha teach test-and-set! call access the car of the cell in a state which is
false. We can allow arbitrary delay between the access by P1 and the set! by P2 so long
as P2 views a false state.
Interestingly, this gets messy very quickly. If P2 is delayed in its set! for a sufficiently
long time that P1 calls (mutex 'release), then P2 would be OK. However, imagine
what would happen if P1 first acquired the mutex, then P1 sets the state to false.
Any 3rd process could acquire the mutex!
|#

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell)
  (set-car! cell false))
(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))

;; Ex. 3.47

;; a
(define (make-semaphore n)
  (let ((mutex (make-mutex))
        (count 0))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (cond ((< count n)
                    (set! count (+ count 1))
                    (mutex 'release))
                   (else
                    (mutex 'release)
                    (the-semaphore 'acquire)))) ; retry
            ((eq? m 'release)
             (cond ((= count 0)
                    (mutex 'release))
                   (else
                    (set! count (- count 1))
                    (mutex 'release))))))
    the-semaphore))

(define semaphore (make-semaphore 3))
(semaphore 'acquire) ; count set to 1
(semaphore 'acquire) ; count set to 2
(semaphore 'acquire) ; count set to 3
(semaphore 'acquire) ; acquire mutex, go to else branch, release mutex, try again -> infinite loop
(semaphore 'release) ; count set to 2
(semaphore 'acquire) ; count set to 3

;; b
#|
Is the single mutex sufficient to protect the count variable?
Can any other process access or set! count after the mutex has been acquired?

In essence, we have 2 states which must be false in order to acquire the semaphore.
It is simplest and safest to lock access to the inner state (count) behind
a mutex so that any change to the inner state is always secure.
|#
(define (atomic-add! cell x)
  (without-interrupts
   (lambda ()
     (set-cdr! cell (+ (cdr cell) x)))))
(define (make-semaphore n)
  (let ((cell (cons false 0)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-semaphore 'acquire)) ; retry mutex acquire
             (cond ((< (cdr cell) n)
                    ;; (set-cdr! cell (+ (cdr cell) 1))
                    ;; to be safe:
                    (atomic-add! cell 1)
                    (clear! cell))
                   (else
                    (clear! cell)
                    (the-semaphore 'acquire)))) ;; retry semaphore due to count >= n
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-semaphore 'release)) ;; retry mutex acquire
             (cond ((= (cdr cell) 0)
                    (clear! cell))
                   (else
                    ;; (set-cdr! cell (- (cdr cell) 1))
                    ;; to be safe:
                    (atomic-add! cell -1))))))
    the-semaphore))


;; Ex. 3.48
#|
The use of unique identification numbers works for the exchange problem as it
provides an ordering for the serialized execution of a sequence of protected
procedures. In essence, for any two arguments, the ordering is always the same,
thus, once the first serialized procedure is entered, no other concurrent exchange
processes may begin to acquire mutexes. This ensures that all mutexes after the
first are available to be acquired by whichever concurrently running exchange
process happened to acquire the first mutex.

See p. 86-87 for a few examples which demonstrate that this works.
|#

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer))
        (id1 (account1 'id))
        (id2 (account2 'id)))
    (cond ((< id1 id2)
           ((serializer1 (serializer2 exchange))
            account1
            account2))
          ((> id1 id2)
           ((serializer2 (serializer1 exchange))
            account1
            account2))
          ((= id1 id2) ; if identical, then the inner mutex 'acquire will never proceed
           ((serializer1 exchange)
            account1
            account2)))))

(define (make-account-and-serializer balance id)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'id) id)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))


;; Ex. 3.49
#|
Condition for deadlock-avoidance mechanism to fail: acquisition on some process must
be out of order.
Or, as the hint says: acquire some resources, then realize that the additional resources
(with lower ids) are needed.

If the resources-to-acquire are computed form the already-acquired resources,
then it is always possible to satisfy the deadlock avoidance mechanism --
perhaps with greater effort than was required to solve the exchange problem,
and perhaps necessitating considerable forward-propagation of partial computations,
but it should be possible.
In other words, once the resources have been acquired, the action to be taken
(either complete execution or acquire more resources) is determined.
However, if we introduce randomness (true randomness, as we could apply the forward-propagation
to a PRNG), then it is not possible to control the ordering, hence, we could produce a mutex
acquisition sequence which is not in ascending order.


If we permit the introduction of true randomness (e.g. by seeding a PRNG with
information from a random source, with the seed acquired /during/ execution of
the procedure), then we truly cannot determine the appropriate ordering to avoid deadlock.
In other words, make one or more critical decision points truly random, thus
leading to the possibility (N.B. not guarantee) of deadlock.

In a more practical context, we need have true randomness, but simply
random decision points, or even a poorly-designed algorithm which does not
expose the necessary decision points to a serializer. Hence, in practice,
deadlock is quite easy to provoke.

|#

#|
To avoid deadlock when serializing, one
1) removes duplicates (easy given ids)
2) orders serializer calls in ascending order
|#
(define (balances-sum-to-value? val . accounts)
  (define (iter result accounts)
    (if (null? accounts)
        false
        (let ((new-val (+ result ((car accounts) 'balance))))
          (if (>= new-val val)
              true
              (iter new-val (cdr accounts))))))
  (iter 0 accounts))
