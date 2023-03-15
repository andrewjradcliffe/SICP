;; construct the factorial procedure and skip over code for procedure body
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; begin actual procedure body
;; compute (= n 1)
  (assign val (const 1))
  (assign arg2 (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign arg1 (reg val))
  (assign val (op =) (reg arg1) (reg arg2))
;; test and branch
  (test (op false?) (reg val))
  (branch (label false-branch4))
true-branch5
  (assign val (const 1))
  (goto (reg continue))
false-branch4
;; compute and return (* (factorial (- n 1)) n)
  (save continue)
;; as open-coded operators are computed from right-to-left, get n from env
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign arg2 (reg val))
;; save arg2 prior to computation of (factorial (- n 1))
  (save arg2)
;; set up (factorial (- n 1)) computation
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
;; compute (- n 1)
  (assign val (const 1))
  (assign arg2 (reg val))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign arg1 (reg val))
  (assign val (op -) (reg arg1) (reg arg2))
;; place (- n 1) into argl
  (assign argl (op list) (reg val))
;; test and go to compiled branch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
compiled-branch7
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch8
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call6
;; restore arg2 (n) from open-coded * operator, then compute (* result arg2)
  (restore arg2)
  (assign arg1 (reg val))
  (restore continue)
  (assign val (op *) (reg arg1) (reg arg2))
  (goto (reg continue))
after-if3
after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
