;; The amb evaluator, based on the analyzing evaluator which includes
;; the syntax from 4.1.2, better handling of internal definitions (4.1.6) and letrec.
#|
In essence, begin from the most advanced analyzing evaluator.
|#

(load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/the-vanilla-with-extra-almond-evaluator.scm")

(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ;;;;;;;;;;;;;;;; amb-related
        ((amb? exp) (analyze-amb exp))
        ;;;;;;;;;;;;;;;; start of extra syntax
        ((and? exp) (analyze (and->if exp)))
        ((or? exp) (analyze (or->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((let*? exp) (analyze (let*->nested-lets exp)))
        ((letrec? exp) (analyze (letrec->let exp)))
        ;; Better handling of not than using implementation language primitive
        ((not? exp) (analyze (not->if exp)))
        ;;;;;;;;;;;;;;;; end of extra syntax
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

;; Before we analyze the sequence, we must scan out the defines
(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (scan-out-defines (lambda-body exp)))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;; Must re-define to overwrite definition used by non-analyzing evaluator.
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         (lambda (a-value fail2)
           (b env succeed fail2))
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

#|
Interestingly, scanning out internal definitions has an interesting
effect here: when we go to save the old-value for an as-to-yet unassigned
variable, lookup-variable-value will throw an error.
This occurs only for the amb evaluator due to the need to save the old values.
The options are: avoid internal definitions, or turn off errors for '*unassigned*.

After some thought, to error is the correct decision. Otherwise, it is possible
for backtracking to lead to a variable being assigned when it should not otherwise be.

A careful reading of p. 430 indicates that while the above is tight reasoning,
variable lookup should always "succeed". Admittedly, simply storing *unassigned*
is safe, and if one then proceeds to use it, any subsequent computation will
almost assuredly error (but there is no guarantee!).
|#
(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             ;; (if (eq? '*unassigned* (car vals))
             ;;     (error "Unassigned variable" var)
             ;;     (car vals))
             (car vals)
             )
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))



(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
       (lambda (arg fail2)
         (get-args (cdr aprocs)
                   env
                   (lambda (args fail3)
                     (succeed (cons arg args)
                              fail3))
                   fail2))
       fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define input-prompt ";;; Amb-Eval input:")
(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

;;;;;;;;;;;;;;;; Tests
;; (driver-loop)
;; (list (amb 1 2 3) (amb 'a 'b))
;; (define (require p)
;;   (if (not p) (amb)))
;; (define (an-element-of items)
;;   (require (not (null? items)))
;;   (amb (car items) (an-element-of (cdr items))))
;; ;;;; Ex. 4.35-4.37
;; ;; For use with (pythagorean-triples).
;; ;; Notably, the amb version (Ben) of pythagorean-triples is considerably faster
;; ;; than the stream-based version.
;; (define (an-integer-starting-from n)
;;   (amb n (an-integer-starting-from (+ n 1))))
;; (load "~/aradclif/scheme-projects/SICP/Chapter4/Evaluators/compound-procedures.scm")
;; ;;;; Ex. 4.44
;; (define empty-board '())
;; (define (adjoin-position new-row k rest-of-queens)
;;   (append rest-of-queens (list (list new-row k))))

;; (define (same-row? x y) (= (car x) (car y)))
;; (define (same-col? x y) (= (cadr x) (cadr y)))
;; (define (same-diagonal? x y)
;;   (let ((di (- (car x) (car y)))
;;         (dj (- (cadr x) (cadr y))))
;;     (= (abs di) (abs dj))))
;; (define (safe-or-identical? x y)
;;   (if (equal? x y)
;;       true
;;       (and (not (same-row? x y)) (not (same-col? x y)) (not (same-diagonal? x y)))))
;; ;; (safe-or-identical? '(1 1) '(2 3))
;; (define (safe? k positions)
;;   (let ((queen (list-ref positions k)))
;;     (accumulate (lambda (x y) (and (safe-or-identical? queen x) y))
;;                 true
;;                 positions)))

;; (define (display-queen-row k row)
;;   (for-each (lambda (x) (display (if (= x k) "■\t" "⋅\t"))) row)
;;   (newline))
;; (define (display-queens positions)
;;   (newline)
;;   (display positions)
;;   (newline)
;;   (newline)
;;   (let ((row (car (transpose positions))))
;;     (for-each (lambda (k) (display-queen-row k row)) (enumerate-interval 1 (length row)))))

;; (display-queens (queens 8))

;; ;;;;;;;;;;;;;;;; Parsing natural language
;; (define nouns '(noun student professor cat class))
;; (define verbs '(verb studies lectures eats sleeps))
;; (define articles '(article the a))
;; (define prepositions '(prep for to in by with))

;; (define *unparsed* '())
;; (define (parse input)
;;   (set! *unparsed* input)
;;   (let ((sent (parse-sentence)))
;;     (require (null? *unparsed*))
;;     sent))
;; (define (parse-word word-list)
;;   (require (not (null? *unparsed*)))
;;   (require (memq (car *unparsed*) (cdr word-list)))
;;   (let ((found-word (car *unparsed*)))
;;     (set! *unparsed* (cdr *unparsed*))
;;     (list (car word-list) found-word)))

;; (define (parse-sentence)
;;   (list 'sentence
;;         (parse-noun-phrase)
;;         (parse-verb-phrase)))

;; (define (parse-prepositional-phrase)
;;   (list 'prep-phrase
;;         (parse-word prepositions)
;;         (parse-noun-phrase)))

;; (define (parse-verb-phrase)
;;   (define (maybe-extend verb-phrase)
;;     (amb verb-phrase
;;          (maybe-extend (list 'verb-phrase
;;                              verb-phrase
;;                              (parse-prepositional-phrase)))))
;;   (maybe-extend (parse-word verbs)))

;; (define (parse-simple-noun-phrase)
;;   (list 'simple-noun-phrase
;;         (parse-word articles)
;;         (parse-word nouns)))

;; (define (parse-noun-phrase)
;;   (define (maybe-extend noun-phrase)
;;     (amb noun-phrase
;;          (maybe-extend (list 'noun-phrase
;;                              noun-phrase
;;                              (parse-prepositional-phrase)))))
;;   (maybe-extend (parse-simple-noun-phrase)))


;; (parse '(the professor lectures to the student in the class with the cat))
