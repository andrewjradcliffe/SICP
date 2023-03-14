  (save continue)    ; unnecessary
  (save env)         ; unnecessary
  (save continue)    ; unnecessary
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (restore continue)
  (goto (label after-lambda1))
entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))
  (save continue)
  (save env)
  (save continue)    ; unnecessary
  (save env)         ; unnecessary
  (save continue)    ; unnecessary
  (assign proc (op lookup-variable-value) (const =) (reg env))
  (restore continue)    ; unnecessary
  (restore env)         ; unnecessary
  (restore continue)    ; unnecessary
  (save continue)       ; unnecessary
  (save proc)           ; unnecessary
  (save env)            ; unnecessary
  (save continue)       ; unnecessary
  (assign val (const 1))
  (restore continue)
  (assign argl (op list) (reg val))
  (restore env)
  (save argl)           ; unnecessary
  (save continue)       ; unnecessary
  (assign val (op lookup-variable-value) (const n) (reg env))
  (restore continue)    ; unnecessary
  (restore argl)        ; unnecessary
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)        ; unnecessary
  (restore continue)    ; unnecessary
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch17))
compiled-branch16
  (assign continue (label after-call15))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch17
  (save continue)    ; unnecessary -- really, truly useless
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue)    ; unnecessary
after-call15
  (restore env)
  (restore continue)
  (test (op false?) (reg val))
  (branch (label false-branch4))
true-branch5
  (save continue)    ; unnecessary -- again, completely useless
  (assign val (const 1))
  (restore continue) ; unnecessary
  (goto (reg continue))
false-branch4
  (save continue)    ; these two save/restore of continue are truly senseless
  (save env)    ; unnecessary
  (save continue)    ; unnecessary
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (restore continue) ; useless
  (restore env) ; useless
  (restore continue) ; useless
  (save continue)    ; we do need to save continue and proc=*
  (save proc)        ; ok
  (save env) ; useless
  (save continue) ; useless
  (assign val (op lookup-variable-value) (const n) (reg env))
  (restore continue) ; useless
  (assign argl (op list) (reg val))
  (restore env) ; useless
  (save argl) ; we do need to save argl
  (save continue) ; useless
  (save env) ; useless
  (save continue) ; useless
  (assign proc (op lookup-variable-value) (const factorial) (reg env))
  (restore continue) ; useless
  (restore env) ; useless
  (restore continue) ; useless
  (save continue) ; we do need to save proc=factorial and continue
  (save proc) ; ok
  (save continue) ; useless
  (save env) ; useless
  (save continue) ; useless
  (assign proc (op lookup-variable-value) (const -) (reg env))
  (restore continue) ; useless
  (restore env) ; useless
  (restore continue) ; useless
  (save continue) ; useless
  (save proc) ; useless
  (save env) ; useless
  (save continue) ; useless
  (assign val (const 1))
  (restore continue) ; useless
  (assign argl (op list) (reg val))
  (restore env) ; useless
  (save argl) ; useless
  (save continue) ; useless
  (assign val (op lookup-variable-value) (const n) (reg env))
  (restore continue) ; useless
  (restore argl) ; useless
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ; necessary
  (restore continue) ; necessary
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch8))
compiled-branch7
  (assign continue (label after-call6))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch8
  (save continue) ; useless
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue) ; useless
after-call6
  (assign argl (op list) (reg val))
  (restore proc) ; necessary
  (restore continue) ; useless
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch11))
compiled-branch10
  (assign continue (label after-call9))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch11
  (save continue) ; useless
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue) ; useless
after-call9
  (restore argl) ; necessary
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc) ; necessary
  (restore continue) ; necessary
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch14))
compiled-branch13
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch14
  (save continue) ; useless
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (restore continue) ; useless
  (goto (reg continue))
after-call12
after-if3
after-lambda1
  (restore env) ; useless
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
  (restore continue) ; useless
