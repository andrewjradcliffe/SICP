;; 5.2.2 The Assembler

;; Ex. 5.8
#|
Due to the installation of different procedures for ambiguous symbols,
we have to assume that lookup-label finds the first occurrence of 'here,
thus, the procedure will assign a to 3.

See p. 197 for step-by-step construction of insts and labels by extract-labels.
|#

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (if (assoc next-inst labels)
                                  (error "ambiguous label -- EXTRACT-LABELS" next-inst)
                                  (receive insts
                                      (cons (make-label-entry next-inst insts)
                                            labels)))
                              (receive (cons (make-instruction next-inst)
                                             insts)
                                  labels)))))))
