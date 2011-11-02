(use srfi-1)
(use srfi-19)
(use srfi-27)   ; random-integer
(use srfi-43)   ; vector-swap!

(define-macro (cons-stream a b)
  `(cons ,a (delay ,b)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream 'the-empty-stream)

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-null? stream)
  (eq? stream the-empty-stream))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;;sicp exercise 3.50
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (show x)
  (newline)
  (display x)
  x)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (square n)
  (* n n))

;; (define (prime? value)
;;   (define (check n)
;;     (cond ((< value (square n)) #t)
;;           ((= 0 (modulo value n)) #f)
;;           (else (check (+ n 2)))))
;;   (cond ((= 1 value) #f)
;;         ((= 2 value) #t)
;;         ((= 0 (modulo value 2)) #f)
;;         (else (check 3))))

(define ones (cons-stream 1 ones))

(define (divisible? x y)
  (= 0 (modulo x y)))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ 1 n))))


(define primes
  (cons-stream
   2 (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes))

(define fib
  (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fib) fib))))

;;(stream-filter (lambda (x) (= 0 (remainder x 3))) (stream-enumerate-interval 1 10))
;;(stream-map show (stream-filter (lambda (x) (= 0 (remainder x 3))) (stream-enumerate-interval 1 10)))
;;(define st (stream-map show (stream-filter (lambda (x) (= 0 (remainder x 3))) (stream-enumerate-interval 1 10000000000000000))))
;; (stream-for-each show (stream-filter prime? (stream-enumerate-interval 1 100)))

;; (stream-for-each show (add-streams (stream-enumerate-interval 1 100) (stream-enumerate-interval 1 100)))
;; (stream-ref fib 10)

;; 3.53
(define s (cons-stream 1 (add-streams s s)))

;; 3.54
(define (mul-stream s1 s2)
  (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-stream integers factorials)))

;;3.55
(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

;;my create
(define (ntn-stream s n)
  (if (< n 1)
      s
      (if (stream-null? s)
          the-empty-stream
          (ntn-stream (stream-cdr s) (- n 1)))))

(define (jump-stream s n)
  (cons-stream (stream-car s)
               (jump-stream (ntn-stream s n) n)))
;3.56
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))

;3.57
;割り算
;(quotient 30 3) => 10

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

;; 割り算を小数点まで表示するよう
(define (stream-ref-iota stream min max)
  (map (lambda (n) (stream-ref stream n)) (iota max min)))

;; (stream-ref-iota (expand 3 8 10) 0 10)

;;3.59
;; (stream-ref-iota (stream-map / ones integers) 0 10)

(define (integrate-series stream)
  (stream-map * stream (stream-map / ones integers)))
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 (scale-stream (integrate-series sine-series) -1)))

(define sine-series
  (cons-stream 0 (integrate-series  cosine-series)))

(stream-ref-iota sine-series 0 10)
(stream-ref-iota cosine-series 0 10)

(define (partial-muls s)
  (cons-stream (stream-car s)
               (mul-stream (stream-cdr s)
                            (partial-muls s))))

;; (stream-ref-iota (partial-muls (scale-stream ones -1)) 0 10)

(define (sin-stream n)
  (stream-map * sine-series (cons-stream 1 (partial-muls (scale-stream ones n)))))

(define (cosine-stream n)
  (stream-map * cosine-series (cons-stream 1 (partial-muls (scale-stream ones n)))))


;; (exact->inexact (stream-ref (partial-sums (sin-stream 1)) 10))
;; (exact->inexact (stream-ref (partial-sums (cosine-stream 1)) 10))

;; (stream-ref-iota (partial-muls (scale-stream ones 2)) 0 10)
;; (stream-ref-iota sine-series 0 10)

(define (get-sin x)
  (exact->inexact (stream-ref (partial-sums (sin-stream x)) 10)))
(define (get-cosine x)
  (exact->inexact (stream-ref (partial-sums (cosine-stream x)) 10)))

;; (stream-ref-iota  (cons-stream 1 (partial-muls (scale-stream ones 2))) 0 10)
;; (stream-ref-iota sine-series 0 10)
;; (exact->inexact (fold + 0 (stream-ref-iota (sin-stream 1) 0 30)))

;;3.61
;; (1 2 3) * (1 2 3)
;; =
;; (1 1*2 1*3)
;; (2 2*2 2*3)
;; (3 3*2 3*3)

;; (1        )
;; (         )
;; (         )

;; (  1*2    )
;; (2 2*2    )
;; (         )

;; こっちじゃないのか？
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                                         (scale-stream (stream-cdr s1) (stream-car s2)))
                            (mul-series (stream-cdr s1) (stream-cdr s2)))))

;;gosh>(stream-ref-iota (partial-sums (mul-series ones ones)) 0 10)
;;(1 4 9 16 25 36 49 64 81 100)

;;こっちが正解
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                                         (scale-stream (stream-cdr s1) (stream-car s2)))
                            (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))
;; gosh> (stream-ref-iota (partial-sums (mul-series ones ones)) 0 10)
;; (1 3 6 10 15 21 28 36 45 55)

;; (stream-ref (add-streams (mul-series (sin-stream 2) (sin-stream 2))
;;                          (mul-series (cosine-stream 2) (cosine-stream 2))) 3)

;;3.61 ピンとこない
(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream  (mul-series (stream-cdr s)
                                          (invert-unit-series s)) -1)))
;; (stream-ref-iota invert-unit-series 0 10)

;; 3.62 skip

;3.5.3
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve quess x)
  (average quess (/ x quess)))

(define (sqrt-stream x)
  (cons-stream 1 (stream-map (lambda (e) (sqrt-improve e x))
                             (sqrt-stream x))))

;; (stream-ref-iota (stream-map exact->inexact (sqrt-stream 2)) 0 10)

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(stream-ref-iota (pi-summands 1) 0 10)

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))
(stream-ref-iota pi-stream 0 10)

;;3.64
(define (stream-limit s limit)
  (let ((v1 (stream-ref s 0))
        (v2 (stream-ref s 1)))
    (if (< (abs (- v2 v1)) limit)
        (cons-stream v2(stream-limit (stream-cdr s) limit))
        (stream-limit (stream-cdr s) limit))))

;; (define (sqrt x tolerance)
;;   (stream-limit (stream-map exact->inexact (sqrt-stream x)) tolerance))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map  (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
    (pairs (stream-cdr s)
           (stream-cdr t)))))

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

;;3.67

(define (pairs2 s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (interleave
     (stream-map  (lambda (x) (list (stream-car s) x))
                  (stream-cdr t))
     (stream-map  (lambda (x) (list x (stream-car t) ))
                  (stream-cdr s)))
    (pairs2 (stream-cdr s)
            (stream-cdr t)))))

(define (triple s1 s2 s3)
  (stream-map (lambda (x) (cons (car x) (cadr x)))
              (pairs2 s1 (pairs2 s2 s3))))

;; (stream-ref-iota (stream-map (lambda (x)
;;                                (show x))
;;                              (stream-filter (lambda (x)
;;                                               (=
;;                                                (+ (square (car x))
;;                                                   (square (cadr x)))
;;                                                (square (caddr x))))
;;                                             (triple integers integers integers)))
;;                  0
;;                  4)

;; 3.70

;; 同じweightの値は消えてしまう。
(define (merge-weighted proc s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (s1weight (proc (stream-car s1)))
               (s2weight (proc (stream-car s2))))
           (cond ((< s1weight s2weight)
                  (cons-stream s1car (merge-weighted proc (stream-cdr s1) s2)))
                 ((> s1weight s2weight)
                  (cons-stream s2car (merge-weighted proc s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car (merge-weighted proc (stream-cdr s1) (stream-cdr s2)))))))))


(define (merge-weighted proc s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2))
               (s1weight (proc (stream-car s1)))
               (s2weight (proc (stream-car s2))))
           (cond ((< s1weight s2weight)
                  (cons-stream s1car (merge-weighted proc (stream-cdr s1) s2)))
                 (else ;;
                  (cons-stream s2car (merge-weighted proc s1 (stream-cdr s2)))))))))



(define (weighted-pairs  proc s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted proc
    (stream-map  (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
    (weighted-pairs proc (stream-cdr s)
                    (stream-cdr t)))))

;; (stream-ref-iota (weighted-pairs (lambda (x)
;;                                    (let ((i (car x))
;;                                      (j (cadr x)))
;;                                      (+ i j)))
;;                  integers integers) 0 100)

;; (stream-ref-iota (weighted-pairs (lambda (x)
;;                                    (let ((x1 (car x))
;;                                      (x2 (cadr x)))
;;                                      (if (> x1 x2)
;;                                          x1
;;                                          (+ x1 x2))))
;;                  integers integers) 0 100)

;; (stream-ref-iota (weighted-pairs (lambda (x)
;;                                    (let ((i (car x))
;;                                      (j (cadr x)))
;;                                      (if (> i j)
;;                                          i
;;                                          (+ (* 2 i) (* 3 j) (* 5 i j)))))
;;                  integers integers) 0 100)

(define integers-no-remainder-2-3-5
  (stream-filter (lambda (x)
                   (not (or (= 0 (remainder x 2))
                            (= 0 (remainder x 3))
                            (= 0 (remainder x 5)))))
                 integers))



;; (stream-ref-iota (weighted-pairs (lambda (x)
;;                                    (let ((i (car x))
;;                                      (j (cadr x)))
;;                                          (+ (* 2 i) (* 3 j) (* 5 i j))))
;;                  integers-no-remainder-2-3-5 integers-no-remainder-2-3-5) 0 100)

;; (stream-ref-iota integers-no-remainder-2-3-5 0 10)


;;3.71 
(define i3_j3
  (weighted-pairs (lambda (x)
                    (let ((i (car x))
                          (j (cadr x)))
                      (+ (* i i i) (* j j j))))
                 integers integers))

;; (stream-ref-iota (stream-map (lambda (x)
;;               (let ((i (car x))
;;                     (j (cadr x)))
;;               (list (+ (* i i i) (* j j j)) x)))
;;             i3_j3)
;;                  0 100)

(define (filter-ramanujan s)
  (let ((x1 (stream-car (stream-ref s 0)))
        (x2 (stream-car (stream-ref s 1))))
    (if (= x1 x2)
        (cons-stream x1 (filter-ramanujan (stream-cdr s)))
        (filter-ramanujan (stream-cdr s)))))

(define ramanujan-numbers
  (filter-ramanujan (stream-map (lambda (x)
              (let ((i (car x))
                    (j (cadr x)))
              (list (+ (* i i i) (* j j j)) x)))
                                i3_j3)))


(stream-ref-iota ramanujan-numbers 0 5)

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(stream-ref-iota (integral ones 1 0.1) 0 10)

;;

(define (RC R C second)
  (lambda (i v0)
    (add-streams (scale-stream i R)
                 (integral (scale-stream ones (/ 1 C)) v0 second)))
  )

(define RC1 (RC 5 1 0.5))
(RC1 ones 1)
(stream-ref-iota (RC1 ones 1) 0 10)

(stream-ref-iota (RC1 (RC1 ones 1) 1) 0 10)


;;3.74

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

;;(define zero-crossings (make-zero-crossings sense-data 0))

(define (sign-change-detector value last-value)
  (cond ((and (> 0 value) (< 0 last-value)) 1)
        ((and (< 0 value) (> 0 last-value)) -1)
        (else 0)))

;;3.75
;; (define (make-zero-crossings input-stream last-value last-avpt)
;;   (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
;;     (cons-stream (sign-change-detector avpt last-avpt)
;;                  (make-zero-crossings (stream-cdr input-stream)
;;                                       (stream-car input-stream)
;;                                       avpt))))
;; 3.76
(define (smooth input-stream)
  (define (iter input-stream last-value)
    (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
      (cons-stream avpt
                   (iter (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      ))))
  (iter (stream-cdr input-stream) (stream-car input-stream)))

(define (smooth input-stream)
  (stream-map (lambda (x y) (/ (+ x y) 2)) input-stream (stream-cdr input-stream)))


;; (make-zero-crossings (smooth (stream-map sin integers)) 0)
;; (stream-ref-iota (make-zero-crossings (smooth (stream-map sin integers)) 0) 0 10)
;; (stream-ref-iota (make-zero-crossings (smooth (stream-map sin integers)) 0) 0 10)
;; (stream-ref-iota (stream-map sin integers) 0 10)

;; (stream-ref-iota (add-streams integers (stream-cdr integers)) 0 10)

;; (stream-ref-iota (stream-map
;;                   (lambda (x) (* 10 (sin x)))
;;                   (stream-map (lambda (x) (* x (/ 3.1415 180))) integers)) 0 100)

;;delay integral
(define (integral deleyed-integral initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force deleyed-integral)))
                 (add-streams (scale-stream integrand dt)
                              int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))  ;;sicpとは逆にする for gauche
  y)

;3.77
(define (integral deleyed-integral initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force deleyed-integral)))
               (if (stream-null? integrand)
                   the-empty-stream
                   (integral (delay (stream-cdr integrand))
                             (+ (* dt (stream-car integrand))
                                initial-value)
                             dt)))))

;;3.78
(define (solve-2nd a b dt y0 dy0)
  (define ddy
    (add-streams (scale-stream dy a)
                 (scale-stream y b)))
  (define dy (integral (delay ddy) dy0 dt))
  (define y (integral (delay dy) y0 dt))
  y)

(random-integer 10)

(define random-init 10)
(define (random-update x)
  ;; ((random-source-make-integers (make-random-source)) x))
  (random-integer 10000000000000000))
(define random-numbers
  (cons-stream random-init
               (stream-map random-update random-numbers)))


(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        random-numbers))


(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p)))
              (monte-carlo cesaro-stream 0 0)))

;; (stream-ref pi 1000)
;; (stream-ref pi 2000)
;; (stream-ref pi 10000)

