#lang planet neil/sicp
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) 
  (let ((g (gcd n d))
        (sign (* (/ n (abs n)) (/ d (abs d)))))
    (cons (* sign (/ (abs n) g)) (/ (abs d) g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment line-seg)
  (car line-seg))

(define (end-segment line-seg)
  (cdr line-seg))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment line-seg)
  (let ((p1 (start-segment line-seg))
        (p2 (end-segment line-seg)))
    (make-point (/ (+ (x-point p1) (x-point p2)) 2)
                (/ (+ (y-point p1) (y-point p2)) 2))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;exercise 2.3
(define (make-rectangle top-left length breadth)
  (cons top-left (cons length breadth)))

(define (length-rect rect)
  (cadr rect))

(define (breadth-rect rect)
  (cddr rect))

(define (area rect)
  (* (length-rect rect)
     (breadth-rect rect)))

(define (perimeter rect)
  (* 2 (+ (length-rect rect)
          (breadth-rect rect))))

;exercise 2.5
(define (my-cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (pow base index)
  (if (eq? 0 index) 1
      (* base (pow base (- index 1)))))

(define (my-car p)
  (if (eq? 0 (remainder p 2))
      (+ 1 (my-car (quotient p 2))) 0))

(define (my-cdr p)
  (if (eq? 0 (remainder p 3))
      (+ 1 (my-cdr (quotient p 3))) 0))

;exercise 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (myprint n)
  ((n inc) 0))

(define (myplus a b)
  (lambda (f) (lambda (x) ((b f) ((a f) x)))))

;extended exercise: Interval Arithmetic.
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (eq? (width-interval y) 0)
      (error "divide interval error")
      (mul-interval x 
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;exercise 2.7
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;exercise 2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;exercise 2.9
(define (width-interval x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

;exercise 2.12
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (make-interval (- c (/ (* p c) 100))
                 (+ c (/ (* p c) 100))))

(define (percent i)
  (* (/ (width i) (center i)) 100))


;--------------------------------------------
;exercise 2.17
(define (last-pair l)
  (if (null? (cdr l)) l
      (last-pair (cdr l))))

;exercise 2.18
(define (reverse l)
  (reverse-h l nil))
(define (reverse-h l ans)
  (if (null? l) ans
      (reverse-h (cdr l) (cons (car l) ans))))

;exercise 2.19 --skipped

;exercise 2.20
(define (same-parity x . y)
  (cons x (same-parity-h x y)))

(define (same-parity-h v l)
  (cond ((null? l) nil)
        ((eq? (remainder (car l) 2)
              (remainder v 2))
         (cons (car l) (same-parity-h v (cdr l))))
        (else (same-parity-h v (cdr l)))))

;exercise 2.23
(define (for-each p l)
  (if (null? l) (values)
      (begin
        (p (car l))
        (for-each p (cdr l)))))

;exercise 2.27
(define (deep-reverse l)
  (deep-reverse-h l nil))

(define (deep-reverse-h l ans)
  (cond
    ((null? l) ans)
    ((not (pair? (car l))) 
     (deep-reverse-h (cdr l)
                     (cons (car l) ans)))
    (else (deep-reverse-h (cdr l)
                          (cons (deep-reverse-h (car l) nil)
                                ans)))))