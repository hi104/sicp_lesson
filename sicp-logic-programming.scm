(use srfi-1)
(use srfi-19)
(use srfi-27)
(use srfi-43)

(load "./sicp-stream.scm")

(define (display-stream stream)
  (stream-for-each show stream))
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define false #f)
(define true #t)


;; database

(define table '((dummy (dummy . 1))))

(define (put key key2 value)
  (let ((alist (assoc key2 table)))
    (if alist
        (set-cdr! alist (put-alist key value (cdr alist)))
        (begin
          (set! table (cons (cons key2 '()) table))
          (put key key2 value)))))


(define (put-alist key value alist)
  (let ((alist2 (assoc key alist)))
    (if alist2
        (set-cdr! alist2 value)
        (set! alist (cons (cons key value) alist))))
  alist)

(define (get key key2)
  (let ((alist (assoc key2 table)))
    (if alist
        (let ((alist2 (assoc key alist)))
          (if alist2 (cdr alist2) false))
        false)))

;; frame

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable value)
  (assoc variable value))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))


(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")
(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
           (newline)
           (display output-prompt)
           (display-stream
            (stream-map
             (lambda (frame)
               (instantiate q
                            frame
                            (lambda (v f)
                              (contract-question-mark v))))
             (qeval q (singleton-stream '()))))
           (query-driver-loop)))))


;; expの変数をunbound-var-handlerで取得した値に変更する
;; 例：query 式の変数をframeから取得してコピーする。

(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
                 (copy (binding-value binding))
                 (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

(define (qeval query frame-stream)
  (begin 
    (let ((qproc (get (type query) 'qeval)))
      (if qproc
          (qproc (contents query) frame-stream)
          (simple-query query frame-stream)))))

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
   (lambda (frame)
     (stream-append-delayed
      (find-assertions query-pattern frame)
      (delay (apply-rules query-pattern frame))))
   frame-stream))

;;for and
(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))
(put 'and 'qeval conjoin)

;;for or
(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
                       frame-stream)))))

(put 'or 'qeval disjoin)

;;for not
(define (negate operands frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query operands)
                              (singleton-stream frame)))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))

(put 'not 'qeval negate)

(define (lisp-value call frame-stream)
  (stream-flatmap
   (lambda (frame)
     (if (execute
          (instantiate
           call
           frame
           (lambda (v f)
             (error "Unknown pat var -- LISP-VALUE" v))))
         (singleton-stream frame)
         the-empty-stream))
   frame-stream))
(put 'lisp-value 'qeval lisp-value)

(define (execute exp)
  (apply (eval (predicate exp) interaction-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)
(put 'always-true 'qeval always-true)

(define (find-assertions pattern frame)
  (stream-flatmap (lambda (datum)
                    (check-an-assertion datum pattern frame))
                  (fetch-assertions pattern frame)))

(define (check-an-assertion assertion query-pattern query-frame)
  (let ((match-result
         (pattern-match query-pattern assertion query-frame)))
    (if (eq? match-result 'failed)
        the-empty-stream
        (singleton-stream match-result))))

(define (pattern-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? pat dat) frame)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((and (pair? pat) (pair? dat))
         (pattern-match (cdr pat)
                        (cdr dat)
                        (pattern-match (car pat)
                                       (car dat)
                                       frame)))
        (else 'failed)))

(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
        (pattern-match (binding-value binding) dat frame)
        (extend var dat frame))))

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (begin
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result
           (unify-match query-pattern
                         (conclusion clean-rule)
                         query-frame)))
      (if (eq? unify-result 'failed)
          the-empty-stream
          (qeval (rule-body clean-rule)
                 (singleton-stream unify-result)))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp)
             (make-new-variable exp rule-application-id))
            ((pair? exp)
             (cons (tree-walk (car exp))
                   (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (begin
    (cond ((eq? frame 'failed) 'failed)
          ((equal? p1 p2) frame)
          ((var? p1) (extend-if-possible p1 p2 frame))
          ((var? p2) (extend-if-possible p2 p1 frame))
          ((and (pair? p1) (pair? p2))
           (unify-match (cdr p1) (cdr p2)
                        (unify-match
                         (car p1)
                         (car p2)
                         frame)))
          (else 'failed))))

(define (extend-if-possible variable val frame)
  (let ((binding (binding-in-frame variable frame)))
    (cond (binding
           (unify-match
            (binding-value binding) val frame))
          ((var? val)
           (let ((binding (binding-in-frame val frame)))
             (if binding
                 (unify-match
                  variable (binding-value binding) frame)
                 (extend variable val frame))))
          ((depends-on? val variable frame)
           'failed)
          (else (extend variable val frame)))))

(define (depends-on? exp variable frame)
  (define (tree-walk e)
    (cond ((var? e)
           (if (equal? variable e)
               true
               (let ((b (binding-in-frame e frame)))
                 (if b
                     (tree-walk (binding-value b))
                     false))))
           ((pair? e)
            (or (tree-walk (car e))
                (tree-walk (cdr e))))
           (else false)))
    (tree-walk exp))

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s THE-ASSERTIONS)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
   (get-stream (index-key-of pattern) 'rule-stream)
   (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
               (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
                 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;; stream
(define (stream-append-delayed s1 delayed-2)
  (if (stream-null? s1)
      (force delayed-2)
      (cons-stream
       (stream-car s1)
       (stream-append-delayed (stream-cdr s1) delayed-2 ))))

(define (interleave-delayed s1 delayed-2)
  (if (stream-null? s1)
      (force delayed-2)
      (cons-stream
       (stream-car s1)
       (interleave-delayed
        (force delayed-2)
        (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flattern-stream (stream-map proc s)))

(define (flattern-stream stream)
  (if (stream-null? stream)
      the-empty-stream
      (interleave-delayed
       (stream-car stream)
       (delay (flattern-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))

;;
(define (type exp)
  (if (pair? exp)
      (car exp)
      (error "Unknown expression Type" exp)))

(define (contents exp)
  (if (pair? exp)
      (cdr exp)
      (error "Unknown expression Contents" exp)))

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct exps) (car exps))
(define (rest-conjuncts exps) (cdr exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct exps) (car exps))
(define (rest-disjuncts exps) (cdr exps))

(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))
(define (conclusion rule) (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))


;;?symbol => (list ? symbol)
(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp)
         (cons (map-over-symbols proc (car exp))
               (map-over-symbols proc (cdr exp))))
         ((symbol? exp) (proc exp))
         (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
        (list '?
              (string->symbol
               (substring chars 1 (string-length chars))))
        symbol)))

;; variable

(define (var? exp)
  (tagged-list? exp '?))

(define (constant-symbol? exp) (symbol? exp))

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1  rule-counter))
  rule-counter)

(define (make-new-variable variable rule-application-id)
  (cons '? (cons rule-application-id (cdr variable))))

(define (contract-question-mark variable)
  (string->symbol
   (string-append "?"
                  (if (number? (cadr variable))
                      (string-append (symbol->string (caddr variable))
                                     "-"
                                     (number->string (cadr variable)))
                      (symbol->string (cadr variable))))))

(add-rule-or-assertion! '(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(add-rule-or-assertion! '(job (Bitdiddle Ben) (computer wizard)))
(add-rule-or-assertion! '(salary (Bitdiddle Ben) 60000))
(add-rule-or-assertion! '(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(add-rule-or-assertion! '(job (Hacker Alyssa P) (computer programmer)))
(add-rule-or-assertion! '(salary (Hacker Alyssa P) 40000))
(add-rule-or-assertion! '(supervisor (Hacker Alyssa P) (Bitdiddle Ben)))
(add-rule-or-assertion! '(address (Fect Cy D) (Cambridge (Ames Street) 3)))
(add-rule-or-assertion! '(job (Fect Cy D) (computer programmer)))
(add-rule-or-assertion! '(salary (Fect Cy D) 35000))
(add-rule-or-assertion! '(supervisor (Fect Cy D) (Bitdiddle Ben)))
(add-rule-or-assertion! '(address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(add-rule-or-assertion! '(job (Tweakit Lem E) (computer technician)))
(add-rule-or-assertion! '(salary (Tweakit Lem E) 25000))
(add-rule-or-assertion! '(supervisor (Tweakit Lem E) (Bitdiddle Ben)))
(add-rule-or-assertion! '(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(add-rule-or-assertion! '(job (Reasoner Louis) (computer programmer trainee)))
(add-rule-or-assertion! '(salary (Reasoner Louis) 30000))
(add-rule-or-assertion! '(supervisor (Reasoner Louis) (Hacker Alyssa P)))
(add-rule-or-assertion! '(supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(add-rule-or-assertion! '(address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(add-rule-or-assertion! '(job (Warbucks Oliver) (administration big wheel)))
(add-rule-or-assertion! '(salary (Warbucks Oliver) 150000))
(add-rule-or-assertion! '(address (Scrooge Eben) (Weston (Shady Lane) 10)))
(add-rule-or-assertion! '(job (Scrooge Eben) (accounting chief accountant)))
(add-rule-or-assertion! '(salary (Scrooge Eben) 75000))
(add-rule-or-assertion! '(supervisor (Scrooge Eben) (Warbucks Oliver)))
(add-rule-or-assertion! '(address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(add-rule-or-assertion! '(job (Cratchet Robert) (accounting scrivener)))
(add-rule-or-assertion! '(salary (Cratchet Robert) 18000))
(add-rule-or-assertion! '(supervisor (Cratchet Robert) (Scrooge Eben)))
(add-rule-or-assertion! '(address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(add-rule-or-assertion! '(job (Aull DeWitt) (administration secretary)))
(add-rule-or-assertion! '(salary (Aull DeWitt) 25000))
(add-rule-or-assertion! '(supervisor (Aull DeWitt) (Warbucks Oliver)))

(define (query query-pattern)
  (let ((q (query-syntax-process query-pattern)))
    (display-stream
     (stream-map
      (lambda (frame)
;;         (newline)
;;         (display frame)
       (instantiate-frame q frame))
      (qeval q (singleton-stream '()))))))

(define (instantiate-frame q frame)
         (instantiate q
                      frame
                      (lambda (v f)
                        (contract-question-mark v))))

'(
  (display-stream (qeval (query-syntax-process
     '(and (job ?person (computer programmer))
          (address ?person ?where)))
       (singleton-stream '())))
  
  (get-indexed-assertions '(job ?x (computer programmer)))
  (get 'salary 'assertion-stream )
  (stream-ref-iota (get 'job 'assertion-stream ) 0 3)
  (stream-cdr (get 'salary 'assertion-stream ))

  (find-assertions (query-syntax-process '(job ?x (computer ?type))) (singleton-stream '()))
  (display-stream (find-assertions (query-syntax-process '(job ?x (computer ?type))) (singleton-stream '())))

  (index-key-of '(job ?x (computer programmer)))
  
  (query-syntax-process '(and (job ?x ?t)
                              (salary ?x ?amount)))
  (binding-in-frame '(? type) '(((? type) . technician) ((? x) Tweakit Lem E)))
  (assoc '(? type) '(((? type) . technician) ((? x) Tweakit Lem E)))

  (make-new-variable '(? x) 1)

  (find-assertions '(job (? x) (computer programmer)) (singleton-stream '()))
  
  (stream-cdr (find-assertions '(job (? x) (computer programmer)) (singleton-stream '())))
  
  (stream-cdr
   (stream-cdr (find-assertions '(job (? x) (computer programmer)) (singleton-stream '()))))

  (stream-cdr (fetch-assertions '(job ?x (computer programmer)) (singleton-stream '())))


  (stream-car
   (qeval
   (query-syntax-process '(job ?x (computer ?type)))
   (singleton-stream '()))
   )
  )

(define (accumulation-stream func frame-stream acc)
  (if (stream-null? frame-stream)
      acc
      (accumulation-stream func (stream-cdr frame-stream)
                           (func acc (stream-car frame-stream)))))

(define (sum accu-exp frame-stream)
  (begin
    ((sum-function (car accu-exp) (cadr accu-exp)) frame-stream)))

(put 'sum 'qeval sum)

(define (sum-function variable query-pattern)
  (lambda (frame-stream)
    (let ((result (extend variable
                           (accumulation-stream
                            (lambda (acc frame)
                              (+  acc (binding-value (binding-in-frame variable frame))))
                            (qeval query-pattern (singleton-stream '()))
                            0
                            )
                           '())))
      (singleton-stream result))))

(define (contain-list lis compare-func)
  (if (null? lis)
      #f
      (if (compare-func (car lis))
          #t
          (contain-list (cdr lis) compare-func))))


(define (contain-list-eq lis elm)
  (contain-list lis (lambda (e) (eq? e elm))))

(define (query-map-frame-match q frame-stream)
    (stream-map
     (lambda (frame)
       (cons (instantiate-frame q frame)
             frame))
     (qeval q frame-stream)))

(define (distinct q frame-stream)
    (let ((matched '()))
     (stream-map
      (lambda (frame-match)
        (cdr frame-match))
      (stream-filter
       (lambda (e)
         (if (contain-list matched (lambda (a)
                                     (not (eq? 'failed (pattern-match a (car e) (lambda (v f) v) )))))
             #f
             (begin
               (set! matched (cons (car e) matched))
               #t)))
       (query-map-frame-match (car q) frame-stream)))))

(put 'distinct 'qeval distinct)

;;###quote  test code
'(
  (rename-variables-in (query-syntax-process '((wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))))
;;
;; ((wheel (? 4313 person))
;;    (and (supervisor (? 4313 middle-manager) (? 4313 person))
;;         (supervisor (? 4313 x) (? 4313 middle-manager))))
   
  (display-stream
   (distinct (query-syntax-process '(distinct (wheel ?x)))
             (singleton-stream '())))
  
  (query '(distinct (wheel ?x)))
             
  (reverse (fold (lambda (e acc)
                 (if (contain-list-eq acc e)
                     acc
                     (cons e acc))) '() (list 1 2 3 1 2 12 3 3)))
  
  (display-stream
   (let ((matched '()))
     (stream-filter
      (lambda (e)
        (if (contain-list matched (lambda (a)
                                    (not (eq? 'failed (pattern-match a (car e) (lambda (v f) v) )))))
            #f
            (begin
              (set! matched (cons (car e) matched))
              #t)))
      (query-map-frame-match (query-syntax-process '(and (job ?x (computer programmer))
                                                         (salary ?x ?amount)))
                             (singleton-stream '())))))
  (display-stream
   (let ((matched '()))
     (stream-filter
      (lambda (e)
        (if (contain-list matched (lambda (a)
                                    (not (eq? 'failed (pattern-match a (car e) (lambda (v f) v) )))))
            #f
            (begin
              (set! matched (cons (car e) matched))
              #t)))
      (query-map-frame-match (query-syntax-process '(wheel ?x)) (singleton-stream '())))))

  (stream-map
   (lambda (frame)
     (cons (instantiate-frame (query-syntax-process '(and (job ?x (computer programmer))
                                                          (salary ?x ?amount)))
                              frame)
           frame))
   (qeval (query-syntax-process '(and (job ?x (computer programmer))
                                      (salary ?x ?amount))) (singleton-stream '())))

  (let ((lis (list)))
    (contain-list (list 1 2 3 4 5 1 2 3 4)
                  (lambda (e)
                    (eq? e 2))))


  (query '(and (distinct (wheel ?x))
             (job ?x ?y)))

  (query '(and (distinct (wheel ?x))
               (salary ?x ?amount)))

  (query '(sum ?amount (and (distinct (wheel ?x))
                            (salary ?x ?amount))))
  
  (query '(sum ?amount (and (job ?x (computer ?y))
                            (salary ?x ?amount))))

  
  )
;;###quote

(query '(and (job ?x (computer ?y))
             (address ?x (?town ?city ?z))
             (lisp-value > 10 ?z)))

(query '(?x . ?y))

(query '(and (job ?x (computer programmer))
             (salary ?x ?amount)))

(add-rule! (query-syntax-process '(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))))

(add-rule! (query-syntax-process '(rule (lives-near ?person-1 ?person-2)
             (and (address ?person-1 (?town . ?rest-1))
                  (address ?person-2 (?town . ?rest-2))
                  (not (same ?person-1 ?person-2))))))

(add-rule! (query-syntax-process '(rule (same ?x ?x))))


(define (add-eval-rule rule)
  (add-rule! (query-syntax-process rule)))

(add-eval-rule '(rule (append-to-form () ?y ?y)))
(add-eval-rule '(rule (append-to-form (?u . ?v) ?y (?u . ?z))
                      (append-to-form ?v ?y ?z)))

(add-eval-rule '(rule (reverse () ?y ?y)))
(add-eval-rule '(rule (reverse ?x ?y)
                      (reverse ?x ?y ())))
(add-eval-rule '(rule (reverse (?u . ?x) ?y ?a)
                      (and (append-to-form (?u) ?a ?v)
                           (reverse ?x ?y ?v))))
'(
  (query '(reverse (1 2 3 4) ?z ()))
  (query '(reverse ?z (1 2 3 4) ())) ;endless loop
  (query '(reverse (1 2 3 4 5 6 7 8 9 10) ?y))
  )

(query '(lives-near ?x (Hacker Alyssa P)))
(query '(lives-near ?x (Bitdiddle Ben)))
(query '(wheel ?x))

(query '(append-to-form (a b) (c d) ?z))
(query '(append-to-form ?x ?y (a b c d)))

(add-eval-rule '(rule (?x next-to ?y in (?x ?y . ?u))))

(add-eval-rule '(rule (?x next-to ?y in (?v . ?z))
                      (?x next-to ?y in ?z)))

(query '(?x next-to ?y in (1 (2 3) 4)))

(query '(?x next-to 1 in (2 1 3 1)))


(add-eval-rule '(rule (last-pair (?x . ()) (?x))))
(add-eval-rule '(rule (last-pair (?x . ?y) ?z)
                      (last-pair ?y ?z)))

(query '(last-pair (3) ?x))
(query '(last-pair (1 2 3) ?x))
