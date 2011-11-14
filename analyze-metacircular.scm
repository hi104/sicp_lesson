(load "./metacircular.scm")

(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assingnment? exp) (analyze-assingnment exp)) ;変数に代入
        ((definition? exp) (analyze-definition exp)) ;変数を定義
        ((if? exp) (analyze-if exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type --EVAL" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))
(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assingnment exp)
  (let ((var (assingnment-variable exp))
        (vproc (analyze (assingnment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
  'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
  'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))


;; 動作がいまいち把握しづらい。
(define (analyze-sequence exps)
  (define (sequentiallyy proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-proc)
    (if (null? rest-proc)
        first-proc
        (loop (sequentiallyy first-proc (car rest-proc))
              (cdr rest-proc))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence --- ANALYZE"))
    (loop (car procs) (cdr procs))))

;; (let* ((one (sequentiallyy print print))
;;        (two (sequentiallyy one print))
;;        (three (sequentiallyy two print)))
;;   (three 1))
;; 1
;; 1
;; 1
;; 1
;; #<undef>
;; gosh>


(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (pproc env)
                           (map (lambda (aproc) (aproc env))
                                        aprocs)))))


(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
           (apply-primitiv-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
         (else
          (error
           "Unknown procedure type -- EXECUTE APPLICATION"
           proc))))

'(
  (eval '(define (fact x)
           (if (< x 1)
               x
               (+ x (fact (- x 1)))))  the-global-environment)
  (eval '(fact 10) the-global-environment)
  (eval '((lambda (n)
            (+ n 1)) 10)  the-global-environment)
  (analyze '((lambda ()
               (fact 10))))
  (eval '((lambda ()
            (fact 10))) the-global-environment)
  ((analyze '((lambda ()
                1024))) the-global-environment)
   )