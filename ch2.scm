(require (lib "fungraph.ss" "concabs"))
(load "quilting.scm")
(load "ch1.scm")

; Helper functions
(define (square x) (* x x))
(define (cube x) (* x x x))

; Examples from text

(define square-2
  (lambda (n)
    (if (= n 0)
        0
        (+ (square-2 (- n 1))
           (- (+ n n) 1)))))

; Exercises

; 2.1

(define power
  (lambda (base exponent)
    (if (= exponent 1)
        base
        (* base
          (power base (- exponent 1))))))

; 2.2 - Factorial Proof

(define factorial
  (lambda (n)
    (if (n = 1)
        1                      ; Base case: factorial of n is 1
        (* (factorial (- n 1)) ; Induction hypothesis: suppose that (factorial k) is the factorial of K
           n))))               ; for all values 1 < k < n. We know that the factorial of 2 is 2. Taking the factorial of
                               ; 2 as k and 3 as n, then we can say that n * k = factorial n. ???
                               ; Perhaps the answer is just that assuming factorial(k) is correct (e


; 2.3 - Tricky bug
; The bug is that that you can 'miss' the base condition if you have an odd number. In other words
; the base condition is not inevitable.

; 2.4 - One layer thinking
(define square3
  (lambda (n)
    (if (= n 0)
        0
        (if (even? n)
            (* (square3 (/ n 2))
               4)
            (+ (square3 (- n 1))
               (- (+ n n 1)))))))

; 2.5
(define multiply-iter
  (lambda (a b sum)
    (cond ((> 0 a) (- (multiply (- a) b)))
          ((> 0 b) (- (multiply a (- b))))
          ((or (= a 0) (= b 0)) 0)
          ((= b 1) (+ sum a))
          (else (multiply-iter a (- b 1) (+ sum a))))))

(define multiply:
  (lambda (a b)
    (multiply-iter a b 0)))

; 2.6

; a - unsure
(define subtract-the-first
  (lambda (n)
    (if (= n 0)
        0
        (- (subtract-the-first (- n 1))
           n))))

; b. The change in the order of multiplication does not
; matter because multiplication is commutative.

; c. Reversing subtract-the-first gives you the same answer,
; except positive

(define subtract-the-first-reverse
  (lambda (n)
    (if (= n 0)
        0
        (- n (subtract-the-first (- n 1))))))

; 2.7 - sum-integers-from-to
(define sum-integers-from-to
  (lambda (low high)
    (if (> low high)
        0
        (+ (sum-integers-from-to (+ low 1) high)
           low))))

; 2.8 - Variations on sum-of-first

; a - sum-of-squares
(define sum-of-squares
  (lambda (n)
    (if (= n 1) 1
        (+ (sum-of-squares (- n 1))
           (square n)))))

; b - sum-of-cubes
(define sum-of-cubes
  (lambda (n)
    (if (= n 1) 1
        (+ (sum-of-cubes (- n 1))
           (cube n)))))

; c - sum-of-powers
(define sum-of-powers
  (lambda (n p)
    (if (= n 1) 1
        (+ (sum-of-powers (- n 1) p)
           (power n p)))))

; 2.9 sixes
(define sixes
  (lambda (n)
    (cond ((< n 0) (sixes (- n)))  ; Takes care of negatives
          ((< n 10) (if (= n 6)
                        1
                        0))
          (else (if (= (remainder n 10) 6)
                    (+ 1 (sixes (quotient n 10)))
                    (sixes (quotient n 10)))))))

; 2.10
(define odd-digits
  (lambda (n)
    (cond ((< n 0) (odd-digits (- n)))
          ((< n 10) (if (odd? n) 1 0))
          ((odd? (remainder n 10)) (+ 1 (odd-digits (quotient n 10))))
          (else (odd-digits (quotient n 10))))))

; 2.11
(define sum-of-digits
  (lambda (n)
    (cond ((< n 0) (odd digits (- n)))
          ((< n 10) n)
          (else (+ (remainder n 10)
                   (sum-of-digits (quotient n 10)))))))

; 2.12
(define (in-exp-iter n k i)
  (cond ((= (* k (expt 2 n)) i) n)
        ((> (* k (expt 2 n)) i) (in-exp-iter (+ n 1) 1 i))
        ((< (* k (expt 2 n)) i) (in-exp-iter n (+ k 2) i))))
  
(define in-exp
  (lambda (i)
    (if (odd? i)
        0
        (in-exp-iter 1 1 i))))

; 2.13
(define stack-copies-of
  (lambda (times img)
    (if (= times 1)
        img
        (stack img
               (stack-copies-of (- times 1) img)))))

; 2.14
(define quilt
  (lambda (image w h)
    (let ((column (stack-copies-of h image)))
          (if (= w 1)
              column
              (side-by-side column
                            (quilt image (- w 1) h))))))

; 2.15
(define invert-row-iter
  (lambda (img w prev-image)
    (if (= w 0)
        prev-image
        (invert-row-iter (invert img)
                         (- w 1)
                         (side-by-side img prev-image)))))
(define inverted-row
  (lambda (img w)
    (if (= w 0)
        img
        (invert-row-iter (invert img)
                         (- w 1)
                         img))))

(define checkerboard-iter
  (lambda (img height accum-img)
    (if (= height 0)
        accum-img
        (checkerboard-iter (invert img)
                           (- height 1)
                           (stack img accum-img)))))
(define checkerboard
  (lambda (img h w)
    (let ((row (inverted-row (pinwheel img) w)))
      (if (= h 1)
          row
          (stack row
                 (checkerboard-iter (invert row)
                             (- h 1)
                             row))))))


; 2.16
; ----

; # Base case
;
;   Base case is reached where n = 0, because
;   (x^(n+1) - 1) / x - 1     equals
;   (x^1 - 1) / x - 1         equals
;   x - 1 / x - 1             equals
;   1                         (at least where x != 1 and n <= 0)

; # Induction hypothesis
;
;   Assume that foo(x m) = (x^(m + 1) - 1) / (x - 1)
;   where 0 >= m < n
; 
; # Inductive step
;
;   foo(x n) = x^n + foo(x, n-1)
;   foo(x n) = (x^n) + ( (x^(m + 1) - 1) / (x - 1) // Using the induction hypothesis
;   foo(x n) = x^n + (x^n - 1) / (x - 1)     / m = n - 1
;   foo(x n) = (x^(n + 1) - 1 / (x - 1)
;   I think that's basically it: 2^2 + 2^2 = 2^4
;   
; # Conclusion

; 2.17
                                   
      