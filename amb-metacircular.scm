;; gauche

(use srfi-27)   ; random-integer
(use srfi-43)   ; vector-swap!

(load "./metacircular.scm")

(define (eval exp env)
  ((analyze exp) env))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assingnment? exp) (analyze-assingnment exp)) ;変数に代入
        ((definition? exp) (analyze-definition exp)) ;変数を定義
        ((let? exp) (analyze (let-combination exp)))        
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
        ((amb? exp) (analyze-amb exp))
;;         ((require? exp) (analyze-require exp)) sicp 4.54
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp)
         (analyze-application exp))
        (else
         (error "Unknown expression type --EVAL" exp))))

(define (expand_syntax? exp)
  ((car exp)))
(define (let? exp)
  (tagged-list? exp 'let))

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

(define (analyze-assingnment exp)
  (let ((var (assingnment-variable exp))
        (vproc (analyze (assingnment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value
                      (lookup-variable-value var env)))
             (set-variable-value! var val env)
             (succeed 'ok
                      (lambda ()
                        (set-variable-value! var
                                             old-value
                                             env)
                        (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

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


(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

(define (analyze-sequence exps)
  (define (sequentiallyy proc1 proc2)
    (lambda (env succeed fail)
      (proc1 env
             (lambda (a-value fail2)
               (proc2 env succeed fail2))
             fail)))

  (define (loop first-proc rest-proc)
    (if (null? rest-proc)
        first-proc
        (loop (sequentiallyy first-proc (car rest-proc))
              (cdr rest-proc))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence --- ANALYZE"))
    (loop (car procs) (cdr procs))))


;; 動作が把握しづらい。
(define (analyze-application exp)
  (let ((pproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (pproc env
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
          (succeed (apply-primitiv-procedure proc args)
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
           "Unknown procedure type -- EXECUTE APPLICATION"
           proc))))

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices)
             env
             succeed
             (lambda ()
               (try-next (cdr choices))))))
      (try-next cprocs))))

(define (amb-choices exp)
  (cdr exp))

(define (amb? exp)
  (tagged-list? exp 'amb))

;; sicp 4.54
;; (define (require? exp)
;;   (tagged-list? exp 'require))

;; (define (analyze-require exp)
;;   (let ((pproc (analyze (require-predicate exp))))
;;     (lambda (env succeed fail)
;;       (pproc env (lambda (pred-value fail2)
;;                    (if pred-value
;;                        (succeed 'ok fail2)
;;                        (fail2) ;;前にambが呼ばれていた場合はのambのtry-nextがよばれる
;;                        ;;fail2
;;                        ))
;;              fail))))

(define (require-predicate exp)
  (cadr exp))

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-predicate exp)
  (cadr exp))

(define (if-fail-alternative exp)
  (caddr exp))

(define (analyze-if-fail exp)
  (let ((pproc (analyze (if-fail-predicate exp)))
        (alternative (analyze (if-fail-alternative exp))))
      (lambda (env succeed fail)
        (pproc env succeed
               (lambda ()
                 (alternative env succeed fail))))))


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
            (display ";;; Starting anew probelm ")
            (ambeval input
                     the-global-environment
                     ;; ambeval 成功
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     (lambda ()
                       (announce-output
                        ";;;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;　ユーティリティー関数を登録。

(define (ambeval-default exp)
  (ambeval exp
         the-global-environment
         (lambda (value fail)
        (print value)
        )
         (lambda () 'fail)))

(define (ambeval-trynext exp)
  (ambeval exp
         the-global-environment
         (lambda (value fail)
           (print value)
           (print 'next)
           (fail))
         (lambda () 'fail)))

;; amb ユーティリティー関数を登録。
(ambeval-default '(define (require p)
            (if (not p) (amb))))

(ambeval-default '(define (an-element-of list)
                    (if (null? list)
                        (amb)
                        (amb (car list)
                             (an-element-of (cdr list))))))


(display "(ambeval-default '(+ 1 2))")
(newline)

(ambeval-default '(+ 1 2))
(display "(ambeval-default '(require (> 1 2))")
(newline)

(ambeval-default '(require (> 1 2)))
(newline)

(display "(ambeval-trynext '(amb 1 2 3))
          (ambeval-trynext '(an-element-of (list 1 2 3)))")
(newline)
(ambeval-trynext '(amb 1 2 3))
(ambeval-trynext '(an-element-of (list 1 2 3)))

(ambeval-default '(begin

                    (define (an-integer-starting-from n)
                      (amb n (an-integer-starting-from (+ n 1))))
                    
                    (define (an-integer-between low high)
                      (if (< low high)
                          (amb low (an-integer-between (+ low 1) high))
                          (amb low)))

                    ;;名詞
                    (define nouns '(noun student professor cat class))
                    ;;動詞
                    (define verbs '(verb stuies lectures eats sleeps))
                    (define articles '(article the a))

                    (define (parse-sentence)
                      (list 'sentence
                            (parse-noun-phrase)
                            (parse-verb-phrase)))

                    ;; (define (parse-noun-phrase)
                    ;;   (list 'noun-phrase
                    ;;         (parse-word articles)
                    ;;         (parse-word nouns)))


                    ;; 4.47
                    ;; メモ 無限ループ?

                    ;; (define (parse-verb-phrase)
                    ;;   (amb (parse-word verbs)
                    ;;        (list 'verb-phrase
                    ;;              (parse-verb-phrase)
                    ;;              (parse-prepositional-phrase))))



                    (define (parse-verb-phrase)
                      (define (maybe-extend verb-phrase)
                        (amb verb-phrase
                             (maybe-extend (list 'verb-phrase
                                                 verb-phrase
                                                 (parse-prepositional-phrase)))))
                      (maybe-extend (parse-word verbs)))

                    (define (parse-simple-noun-phrase)
                      (list 'simple-noun-phrase
                            (parse-word articles)
                            (parse-word nouns)))


                    (define (parse-noun-phrase)
                      (define (maybe-extend noun-phrase)
                        (amb noun-phrase
                             (maybe-extend (list 'noun-phrase
                                                 noun-phrase
                                                 (parse-prepositional-phrase)))))
                      (maybe-extend (parse-simple-noun-phrase)))


                    (define (parse-word word-list)
                      (require (not (null? word-list)))
                      (require (memq (car *unparsed*) (cdr word-list)))
                      (let ((found-word (car *unparsed*)))
                        (set! *unparsed* (cdr *unparsed*))
                        (list (car word-list) found-word)))

                    (define *unparsed* '())

                    (define (parse input)
                      (set! *unparsed* input)
                      (let ((sent (parse-sentence)))
                        (require (null? *unparsed*))
                        sent))
                    ;; (define (parse-verbs)
                    ;;   (word-list verbs))
                    ;;   )
                    (define prepositions '(prep for to in by with))

                    (define (parse-prepositional-phrase)
                      (list 'prep-phrase
                            (parse-word prepositions)
                            (parse-noun-phrase)))

                    ))
                 

(ambeval-default '(define (a-pythagorean-triple-between low high)
                    (let ((i (an-integer-between low high)))
                      (let ((j (an-integer-between i high)))
                        (let ((k (an-integer-between j high)))
                          (require (= (+ (* i i) (* j j)) (* k k)))
                          (list i j k))))))

(ambeval-trynext '(a-pythagorean-triple-between 1 20))


(ambeval-default '(parse '(the cat eats)))
(ambeval-default '(parse '(the cat cat)))
(ambeval-default '*unparsed*)
(ambeval-default '(parse '(the student with the cat sleeps in the class)))



(define (ambeval-try-parse-next exp)
  (ambeval exp
         the-global-environment
         (lambda (value fail)
           (print value)
           (print 'next)
           (ambeval-default `(set! *unparsed* ,exp))
           (fail))
         (lambda () 'fail)))

(ambeval-try-parse-next '(parse '(the student with the cat sleeps in the class)))
(ambeval-try-parse-next '(parse '(the professor lectures to the student with the cat)))

;; (sentence
;;  (noun-phrase (simple-noun-phrase (article the) (noun student))
;;            (prep-phrase (prep with)
;;                         (noun-phrase (article the) (noun cat))))
;;  (verb-phrase (verb sleeps)
;;            (prep-phrase (prep in)
;;                         (noun-phrase (article the) (noun class)))))

(ambeval-try-parse-next 
 '(parse '(sentence (simple-noun-phrase (article the) (noun professor))
            (verb-phrase
             (verb-phrase (verb lectures)
                          (prep-phrase (prep to)
                                       (simple-noun-phrase (article the) (noun student))))
             (prep-phrase
              (prep with) (simple-noun-phrase (article the) (noun cat)))))
 ))


'(
  (ambeval '(begin
              (define amfn
                (lambda (n)
                  (if (< n 100)
                      (amb n
                           (amfn (+ n 1)))
                      (amb n))))
              (define result (amfn 1))
              (require (= result 10))
              result
              )
           the-global-environment
           (lambda (value fail)
             (print value)
             (print 'next)
             (fail))
           (lambda () 'fail))


  (ambeval '(begin (define t (amb 1 2 3)) (require (= t 4)) t)
           the-global-environment
           (lambda (value fail) (list value fail))
           (lambda () 'fail))

  (ambeval '(if (> 3 6) (+ 1 2) (+ 2 4))
           the-global-environment
           (lambda (value fail) value)
           (lambda () 'fail))

  (ambeval '(let ((x (+ 1 2)) (y (* 2 1))) (+ (+ x y) 2))
           the-global-environment
           (lambda (value fail) (list value fail))
           (lambda () 'fail))

  (ambeval '(let ((x (amb 1 2 3 4 5 6 7 8 9))
                  (y (* 2 1)))
              (require (> x 2))
              x)
           the-global-environment
           (lambda (value fail)
             (print value)
             (print 'next)
             (fail))
           (lambda () 'fail))

  (ambeval '(let ((x (amb 1 2 3 4 5 6 7 8 9 10))
                  (y (amb 1 2 3 4 5 6 7 8 9 10))
                  (z (amb 1 2 3 4 5 6 7 8 9 10))
                  (a (amb 1 2 3 4 5 6 7 8 9 10))
                  (b (amb 1 2 3 4 5 6 7 8 9 10)))
              (require (= (+ x y z a b) (* x y z a b)))
              (list x y z a b))
           the-global-environment
           (lambda (value fail)
             (print value)
             (print 'next)
             (fail))
           (lambda () 'fail))

  (ambeval '(if-fail (let ((x 1))
                       (require (> x 2))
                       x)
                     (if-fail (let ((x 16))
                                (require (> x 2))
                                x)
                              (+ 1 10)))
           the-global-environment
           (lambda (value fail)
             (print value)
             (print 'next)
             (fail))
           (lambda () 'fail))
  )