(define apply-in-underlying-scheme apply)
(define false #f)
(define true #t)
(define eval-debug #f)

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitiv-procedure procedure arguments))
        ((compound-procedure? procedure)
         (begin
           (if eval-debug
           (begin (print "apply")
                  (print (list (procedure-body procedure) (procedure-parameters procedure))))
           '())
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure)))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

;;変数に値を代入。
(define (eval-assingnment exp env)
  (set-variable-value! (assingnment-variable exp)
                       (eval (assingnment-value exp) env)
                       env)
  'ok)

;;変数の定義。
(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)

;;"自己評価式 文字とか数字とか　基本型
(define (self-evaluating? exp)
 (cond ((number? exp) true)
       ((string? exp) true)
       (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp) (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (assingnment? exp) (tagged-list? exp 'set!))

(define (assingnment-variable exp) (cadr exp))
(define (assingnment-value exp) (caddr exp))

;;
;;define
;;

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                  (cddr exp))))
;;
;;lambda
;;

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;;if
;;
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
        (list 'if predicate consequent alternative))

;;
;;begin sequence
;;
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;
;;application
;;

(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
;;
;;cond
;;
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clauses) (car clauses))
(define (cond-actions clauses) (cdr clauses))

(define (cond->if exp)
        (expand-clauses (cond-clauses exp)))

(define (expand-clauses clause)
        (if (null? clauses)
            'false
            (let ((first (car clauses))
                  (rest (cdr clauses)))
              (if (cond-else-clause? first)
                  (if (null? rest)
                      (sequence->exp (cond-actions first))
                      (error "ELSE clause isn't last -- COND->IF"
                             clauses))
                  (make-if (cond-predicate first)
                           (sequence->exp (cond-actions first))
                           (expand-clauses rest))))))

;;p221 問題4.2
;;evalのcond節 を手続き作用(apply?)と代入(assignment?)の節を変更。
;;

;;aの回答
;; application? はpair?で判断するので、applicationの移行のcond節にいかなくなる
;;

;;bの回答
;;cond節に
;;(call? exp)(apply ....) というの追加すればいいのでは 括弧リストの最初で出てくるものは限定される。
;;

;;
;; こうなったらとういうことののか？
;; (define (eval exp env)
;;   (cond ((self-evaluating? exp) exp)
;;      ((variable? exp) (lookup-variable-value exp env))
;;      ((quoted? exp) (text-of-quotation exp))
;;      ((application? exp)  ;;入れ替え
;;       (apply (eval (operator exp) env)
;;              (list-of-values (operands exp) env)))
;;      ((assignment? exp) (eval-assignment exp env)) ;入れ替え
;;      ((definition? exp) (eval-definition exp env))
;;      ((if? exp) (eval-if exp env))
;;      ((lambda? exp)
;;       (make-procedure (lambda-parameters exp)
;;                       (lambda-body exp)
;;                       env))
;;      ((begin? exp)
;;       (eval-sequence (begin-actions exp) env))
;;      ((cond? exp) (eval (cond->if exp) env))

;;      (else
;;       (error "Unknown expression type --EVAL" exp))))


;;問題4.3
;;
;;
;;問題 2.7.3(p108)の部分はやってないのでいまいち問題文がわからないのであとでする。
;; eval のcond部分をリストでやるということか  ;;2010/11/8 tableに追加をしていくよう。 p105~7
;;

;;問題4.4
;;
;;

;;問題4.5
;;
;;expand-clauses関数を変更すればできそう
;;memo (if (eq? (cadr (list 1 '=> 3)) '=>) true false)
;; =>が含まれているかどうか
(define (cond-expand? clauses)
  (let ((expander (cond-actions clauses)))
        (if (pair? expander)
            (eq? '=> (car expander))
            false)))


;;問題4.6
;;
(define (carlist list)
  (if (pair? list)
      (cons (car list) (carlist (cdr list)))
      '()))

;; (define (mmap elm fn)
;;   (if (pair? elm)
;;       (cons (fn (car elm))
;;          (mmap (cdr elm) fn))
;;       '()))
;; (mmap '((1) (2) (3)) (lambda (x) (+ (car x) 1)))


(define (let-combination exp)
  (cons (cons 'lambda
              (cons
               (map car (car (cdr exp)))

     (cdr (cdr exp))))
        (map
      (lambda (x) (cadr x))
          (car (cdr exp)))))

(let-combination '(let ((x (+ 1 2)) (y (* 2 1))) (+ (+ x y) 2)))

;;問題4.7
(define (make-nested->let exp body)
   (if (pair? exp)
       (cons (cons 'lambda
                   (cons (cons (car (car exp)) '())
                         (cons
                          (make-nested->let (cdr exp) body) '())
                         ))
             (cdr (car exp)))
       body))

(define (nested->let exp)
  (make-nested->let (cadr exp) (caddr exp)))

;; (make-nested->let '(
;;                (x 1)
;;                (y (+ x 2))
;;                (z (+ y 3)))
;;              '(+ x y z))

;; (nested->let '(let* (
;;                      (x 1)
;;                      (y (+ x 2))
;;                      (z (+ y 3))
;;                      )
;;                 (+ x y z)))

;; 問題4.8 defineすれば良さそう

(define (name-let-combination exp)
  (cons 'begin
        (cons (cons 'define
                    (cons (cons (car (cdr exp))
                                (map car (car (cddr exp))))
                          (cdr (cddr exp))))
              (list
               (cons (car (cdr exp))
                     (map
                      (lambda (x) (cadr x))
                           (car (cddr exp))))))))

(name-let-combination '(let fib-iter ((a 1)
                                      (b 0)
                                      (count 10))
                         (if (< count 1)
                             b
                             (fib-iter (+ a b) a (- count 1)))))
;;   ((lambda(fn)
;;      (fn fn))
;;    (lambda (f)
;;      (if (> a 0) ;valid state
;;          (begin (set! a (- a 1))  ;; next state
;;                 (print a)  ;; block state
;;                 (f f))
;;          'f))))

(define (make-do start valid next block)
  `(let ,start ((lambda
                    (fn) (fn fn))
                (lambda (f)
                  (if ,valid
                      (begin ,block ,next (f f))
                      '())))))

(define (make-do-block exp)
  (make-do (cadr exp)
           (caddr exp)
           (cadddr exp)
           (car (cddddr exp))))

;; (make-do-block '(do ((a 1) (b 1))
;;          (< a 10)
;;          (begin (set! b (+ b a)) (set! a (+ a 1)))
;;          (print b)))
(make-do '((a 1) (b 1))
         '(< a 10)
         '(begin (set! b (+ b a)) (set! a (+ a 1)))
         '(print b))


;; (define (test first next)
;;      `(,first ,next))
;; test
;; gosh> (test '(let ((a 123)(b 10))) (cons 1 (cons 2 '())))
;; ((let ((a 123) (b 10))) (1 2))


;; (lambda (fn) (if valid (begin (block) (fn fn)) 'f))
;; '(do valid next block)
;; (cadr '(do valid next block))
;; valid
;; (caddr '(do valid next block))
;; next
;; (cadddr '(do valid next block))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

;; procedure
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;; environment

;; gosh> (pair? (list 1))
;; #t
;; gosh> (null? '())
;; #t
;;ここを参照
;;http://practical-scheme.net/gauche/man/gauche-refj_43.html

(define (extend-environment vars vals env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (if (null? vars)
          (env-loop (enclosing-environment env))
          (if (eq? (car vars) var)
              (car vals)
              (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;; gosh> (lookup-variable-value 'x (cons (make-frame 'x 1) '()))
;; 1
;; gosh> (lookup-variable-value 'y (cons (make-frame 'x 1) '()))
;; *** ERROR: Unbound variable y
;; Stack Trace:

(define (define-variable! var val env)
  (define (scan vars vals frame)
    (if (null? vars)
        (add-binding-to-frame! var val frame)
        (if (eq? (car vars) var)
            (set-car! vals val)
            (scan (cdr vars) (cdr vals) frame))))
  (let ((frame (first-frame env)))
    (scan (frame-variables frame)
          (frame-values frame)
          frame))
  env
  )

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals frame)
      (if (null? vars)
          (env-loop (enclosing-environment env))
          (if (eq? (car vars) var)
              (set-car! vals val)
              (scan (cdr vars) (cdr vals) frame))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)
                frame))))
  (env-loop env)
  )

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


(define (add-binding-to-aframe! pair frame)
  (cons pair frame))
(define (make-aframe variables values)
  (cons
   (cons
    (cons variables values) '())
   '()))
(define (aframe-variable pair) (car pair))
(define (aframe-varlue pair) (cdr pair))
(define (lookup-variable-value-alist var env)
  (define (env-loop env)
    (define (scan frame)
      (if (null? frame)
          (env-loop (enclosing-environment env))
          (if (eq? var (car (car frame)))
              (cdr (car frame))
              (scan (cdr frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

;;4.12
;;
;;現在の環境が終わった場合
;;変数が見つかったとき の処理
(define (traverse-frame  find-act end-env-act)
  (lambda (var val env)
    (define (env-loop env)
      (define (scan vars vals frame)
        (if (null? vars)
            (end-env-act var val frame)
            (if (eq? (car vars) var)
                (find-act  frame)
                (scan (cdr vars) (cdr vals) frame))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)
                  frame))))
    (env-loop env)))

(define (traverse-looking-variable var val env)
  ((traverse-frame (lambda (vars vals frame)
                    (env-loop env))
                  (lambda (var val frame)
                    (cdr frame)))
   var val env))

;;4.13
;; delete
;; 現在の環境からunbindするとよいと思う。
;; 他の場所環境を消すと、現在以外の環境に影響を与えてしまい、プログラムの動きが把握しずらい。
;; 正当化

;;4.1.4

(define (setup-enviroment)
  (let ((initial-env
        (extend-environment (primitive-procedure-names)
                            (primitive-procedure-objects)
                            the-empty-environment)))
  (define-variable! 'true true initial-env)
  (define-variable! 'false false initial-env)
  initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'list list)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '> >)
        (list '< <)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define the-global-environment (setup-enviroment))

(define (apply-primitiv-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))
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

;;4.14
;;
;;基本手続きの場合のmapの場合 変数の参照場所が違う場所になる。
;;

(define syntax-extend
  (list (list 'let let-combination)
        (list 'let* nested->let)
        (list 'do make-do-block)
        ))

(define (syntax-names) (map car syntax-extend))

(define (syntax-objects)
  (map (lambda (syn) (list 'syntax (cadr syn)))
       syntax-extend))

(define (add-syntax-environment vars vals env)
  (if (null? vars)
      env
      (begin (define-variable! (car vars) (car vals) env)
             (add-syntax-environment (cdr vars) (cdr vals) env))))

(define (setup-syntax-enviroment)
  (add-syntax-environment
   (syntax-names)
   (syntax-objects)
   the-global-environment))


(define (syntax-extend? exp)
  (tagged-list? exp 'syntax))

(setup-syntax-enviroment)

(define (find-syntax syntax )
  (define (loop syntax list)
    (if (null? list)
        #f
        (if (eq? syntax (car (car list)))
            (cadr (car list))
            (loop syntax (cdr list)))))
  (loop syntax syntax-extend))

(define (available-syntax? exp)
    (if (pair? exp)
        (if (find-syntax (car exp))
            true
            false)
        false))
      
(define (eval exp env)
  (begin
    (if eval-debug ;;debug用
        (print exp) '())
    (cond ((self-evaluating? exp) exp)
          ((variable? exp) (lookup-variable-value exp env))
          ((quoted? exp) (text-of-quotation exp))
          ((assingnment? exp) (eval-assingnment exp env)) ;変数に代入
          ((definition? exp) (eval-definition exp env)) ;変数を定義
          ((if? exp) (eval-if exp env))
          ((lambda? exp)
           (make-procedure (lambda-parameters exp)
                           (lambda-body exp)
                           env))
          ((begin? exp)
           (eval-sequence (begin-actions exp) env))
          ((cond? exp) (eval (cond->if exp) env))
          ((available-syntax? exp) (eval ((find-syntax (car exp)) exp) env))
          ((application? exp)
           (let ((ope (eval (operator exp) env)))
                 (begin
                   (if eval-debug
                       (begin (print "application")
                              (print ope))
                       '())
                   (apply (eval (operator exp) env)
                          (list-of-values (operands exp) env)))))
          (else
           (error "Unknown expression type --EVAL" exp))))
    )


'(
  (eval '(define (fact x)
           (if (< x 1)
               x
               (+ x (fact (- x 1)))))  the-global-environment)

  (eval '(fact 10)  the-global-environment)

  (eval '(define (fact-2 x)
           (+ (fact x) (fact x))) the-global-environment)

  (eval
   '(define (map fn list)
      (if (null? list) '()
          (cons (fn (car list))
                (map fn (cdr list))))) the-global-environment)

  (eval '(map
          (lambda (e) (+ e 1))
          (list 1 2 3 4)) the-global-environment)

  (eval '(let ((x 1)
               (y 1))
           (+ x y))  the-global-environment)

  (eval '(let* ((x 1)
                (y (+ x 1)))
           (+ x y))  the-global-environment)
  )