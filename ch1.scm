#lang planet neil/sicp
; exercise 1.17
(define (fast-expt-h b ans n)
  (cond ((= n 0) ans)
        ((even? n) (fast-expt-h (* b b) ans (/ n 2)))
        (else (fast-expt-h b (* ans b) (- n 1)))))
(define (fast-expt-iter b n)
  (fast-expt-h b 1 n))


;ex 1.22
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))

(define (search-for-primes range)
  (smallest-primes (+ range 1) 3))

(define (smallest-primes m n)
  (cond ((= n 0) '())
        ((prime? m) (cons m (smallest-primes (+ m 2) (- n 1))))
        (else (smallest-primes (+ m 2) n)))) 

;simpson's rule integration
(define (simpson-integration f a b n)
  (+ 0.0 (* (/ (- b a) (* 3 n)) 
            (simpson-integration-h f a b n 0))))

(define (simpson-integration-h f a b n x)
  (cond ((= x n) (f b))
        (else (let* ([f-val (f (+ a (* x (/ (- b a) n))))]
                     [term (cond ((= x 0) (f a))
                                 ((even? x) (* 2 f-val))
                                 ((odd? x) (* 4 f-val)))])
                (+ term (simpson-integration-h f a b n (+ x 1)))))))

(define (id x) x)
(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


;exer 1.30 sum iteratively
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b) 
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

(define (sum-cubes a b)
  (sum-iter cube a inc b))
(define (inc n) (+ n 1))

;ex 1.32 hof for sum and product
(define (accumulate  combiner null-value
                     term a next b)
  (if (> a b)
      null-value
      (combiner (term a )
                (accumulate combiner null-value
                            term (next a) next b))))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (factorial n)
  (product id 1 inc n))

(define (pi-term x)
  (* (/ x (- x 1)) (/ x (+ x 1))))
(define (pi-next x)
  (+ x 2))

(define (pi-approx n)
  (dec-dis (* (/ 8 3) (product pi-term 4 pi-next (+ n 4)))))

(define (dec-dis x)
  (+ .0 x))

;ex 1.33 filtered-accumulate
(define (filtered-accumulate  combiner null-value
                              term a next b filter)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a )
                              (filtered-accumulate combiner null-value
                                                   term (next a) next b filter)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (sum-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    ;(newline)
    ;(display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y) 2))

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1))
;(define x-pow-x-damp (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))
;(define x-pow-x (fixed-point (lambda (x) (/ (log 1000) (log x))) 2))

;exercise 1.37
(define (cont-frac n d k)
  (cont-frac-h 1 n d k))

(define (cont-frac-h i n d k)
  (if (= k 0) i
      (cont-frac-h (/ (n k) (+ (d k) i)) n d (- k 1))))

(define (cont-frac-r i j n d)
  (if (= i j) (/ (n i) (d i))
      (/ (n i) (+ (d i) (cont-frac-r (+ i 1) j n d)))))

;exercise 1.38
;error in approximation. haven't figured out yet.
(define (euler-n i) i)

(define (euler-d i)
  (cond ((= i 1) 1)
        ((= i 2) 2)
        (else (let ((j (- i 2)))
          (if (= (remainder j 3) 0) 
              (* 2 (+ (quotient j 3) 1))
              1)))))

(define (euler-e k) 
  (+ 2.0 (cont-frac euler-n euler-d k)))
              
;exercise 1.39
(define (lambert-n x)
  (lambda (i)
    (if (= i 1) x
        (- (* x x)))))

(define (lambert-d i)
  (- (* 2 i) 1))

(define (tan-cf x k)
  (cont-frac (lambert-n x) lambert-d k))

;exercise 1.40
(define (deriv g)
  (lambda (x) 
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x) 
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))


(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;exercise 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

;exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;exercise 1.43
(define (repeated f n)
  (if (= n 1) f
      (compose (repeated f (- n 1)) f)))

;exercise 1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) 
          (f x) 
          (f (+ x dx))) 3)))

;exercise 1.45
; yet to do


;exercise 1.46
;(define (iterative-improve
;got a long break. skipped it.


    
