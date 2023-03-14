;; construct the factorial procedure and skip over code for procedure body
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))
entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
;; construct the iter procedure and skip over code for procedure body
  (assign val (op make-compiled-procedure) (label entry7) (reg env))
  (goto (label after-lambda6))
entry7
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))
;; begin actual iter procedure body
  (save continue)
  (save env)
;; compute (> counter n)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20    ; val now contains result of (> counter n)
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch9))
true-branch10
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))
false-branch9
;; compute and return (iter (* counter product) (+ counter 1))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (save continue)
  (save proc)    ; save iter procedure
  (save env)     ; save iter environment
;; compute (+ counter 1)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
compiled-branch15
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call14    ; val now contains (+ counter 1)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)    ; save argl for later
;; compute (* counter product)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
compiled-branch12
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call11    ; val now contains (* counter product)
;; finish creating argl, restore iter into proc, then apply iter
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
compiled-branch18
;; assign procedure iter's entry into val and jump to it
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call17
after-if8
after-lambda6
;; assign the procedure to the variable iter
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call3
after-lambda1
;; assign the procedure to the variable factorial
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
