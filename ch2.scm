(require (lib "fungraph.ss" "concabs"))
(load "quilting.scm")

; Helper functions
(define (square x) (* x x))

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
(define square
  (lambda (n)
    (if (= n 0)
        0
        (if (even? n)
            (* (square (/ n 2))
               4)
            (+ (square (- n 1))
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
