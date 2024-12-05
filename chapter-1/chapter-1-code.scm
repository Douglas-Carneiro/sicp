
;;; Naming and the Environment
(define pi 3.14159)
(define radius 10)
(define circumference (* 2 pi radius))

(define x 25)

(define x-plus-2 (+ x 2))

; Compound Procedures
(define (square x) (* x x))

(square (+ 2 5))
(square (square 3))
(define (sum-of-squares x y) (+ (square x) (square y)))

(define (f a) 
    (sum-of-squares (+ a 1) (* a 2)))

; Conditional Expressions and Predicates
(define (abs x)
    (cond ((> x 0) x)
          ((= x 0) 0)
          ((< x 0) (- x))))

; Improved version
(define (abs x)
    (cond ((< x 0) 0)
          (else x)))

; Using 'if'
(define (abs x)
    (if (< x 0)
        (- x)
        x))

;; The book says that 'not' is implemented as a regular procedure
;; This is the implementation I came up with:
(define (my-not x)
  (if (and x #t) #f #t))

(define (>= x y) (not (< x y)))

;;; Example: Square Roots by Newton’s Method
(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
    
(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
    (sqrt-iter 1.0 x))

;;; Exercise 1.6
; Since the interpreter uses aplicative order, the then-clause -> ( (sqrt-iter (improve guess x) x) )
; will be evaluated before it is passed to the procedure new-if, independently of the value of 'predicate',
; this, in turn, will cause an infinite number of calls to sqrt-iter that will only stop once the numbers become too big for the machine to handle.
; Using a normal if the then clause ( (sqrt-iter (improve guess x) x) ) is only executed when the predicate is false.

; (define (new-if predicate then-clause else-clause)
;     (cond (predicate then-clause)
;         (else else-clause)))

; (define (sqrt-iter guess x)
;     (new-if (good-enough? guess x)
;         guess
;         (sqrt-iter (improve guess x) x)))

; Example for exercise 1.6: (new-if #t 1 (display "should not execute"))

;;; Exercise 1.7
; If two very small numbers are passed to good-enough? (guess and x) the condition returns true before the guess
; is improved enough since the precision of the procedure is limited, this leads to wrong results. For example, 
; fails for (sqrt 0.00001) and
; (sqrt-iter 1.0 0.0001)

; For very large numbers the test failed at (sqrt 10000000000000) because when the numbers are very large
; the machine will not be able to operate on the numbers and will not be able to improve the guess, 
; causing an infinite loop


; I just arrived at the same solution as http://community.schemewiki.org/?sicp-ex-1.7
; by reading the problem more carefully at my second time reading the book
(define (good-enough?-improved guess x)
  (<= (abs (- (improve guess x) guess)) (* guess 0.001)))


; My answer (does not work)
; (define (good-enough-improved? guess x)
;     (< (/ (improve guess x) guess) 0.001))

; Answer from http://community.schemewiki.org/?sicp-ex-1.7
(define (good-enough-improved? guess x) 
  (<= (abs (- (improve guess x) guess)) 
     (* guess .001))) 

(define (sqrt-iter-improved guess x)
    (if (good-enough-improved? guess x)
        guess
        (sqrt-iter-improved (improve guess x) x)))

(define (sqrt-2 x)
    (sqrt-iter-improved 1.0 x))

;; The alternative version works better than the previous good-enough? test, for both small and large numbers

;;; Exercise 1.8
(define (cube x) (* x x x))

(define (good-enough-cube? guess x)
    (< (abs (- (cube guess) x)) 0.001))

; Alternative version:
; (define (good-enough-cube? guess x)
;     (<= (abs (- (improve-cube guess x) guess)) 
;      (* guess .0001)))

(define (improve-cube guess x) 
    (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (cube-root-iter guess x)
    (if (good-enough-cube? guess x)
        guess
        (cube-root-iter (improve-cube guess x) x)))

(define (cube-root x) 
    (cube-root-iter 1.0 x))

;; Procedures as Black Box Abstractions
(define (square x) (* x x))
(define (square x) (exp (double (log x))))
(define (double x) (+ x x))

; Internal definitions and block structure

; Block structure:
(define (sqrt x)
    (define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess x) (average guess (/ x guess)))
    (define (sqrt-iter guess x)
        (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
    (sqrt-iter 1.0 x))

; Lexical scoping (x is not passed directly to the internal procedures):
(define (sqrt x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess)
        (average guess (/ x guess)))
    (define (sqrt-iter guess)
        (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
    (sqrt-iter 1.0))

;;; 1.2 Procedures and the Processes They Generate

; "The ability to visualize the consequences of the actions under consideration is crucial to becoming an expert programmer"
; "To become experts, we must learn to visualize the
; processes generated by various types of procedures. Only after we have
; developed such a skill can we learn to reliably construct programs that
; exhibit the desired behavior."

;;; 1.2.1 Linear Recursion and Iteration

; Recursive factorial definition:
(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

; Iterative factorial definition:
(define (factorial n)
    (fact-iter 1 1 n))
(define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* product counter) 
                   (+ counter 1)
                   max-counter)))
; Using block structure:
(define (factorial n)
    (define (iter product counter)
        (if (> counter n)
            product
            (iter (* counter product)
                  (+ counter 1))))
    (iter 1 1))

;;; Exercise 1.9

; Using the substitution model, illustrate the process gener-
; ated by each procedure in evaluating (+ 4 5). Are these
; processes iterative or recursive?

; First procedure:
; (define (+ a b)
;     (if (= a 0) b (inc (+ (dec a) b))))

; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
;; This procedure executes a linear recursive process.
;; It is possible to arrive at this conclusion by observing the expansion in the shape of the procedure execution
;; due to the deferred calls to (inc) and the contraction when the operations are actually executed.

; Second procedure:
;(define (+ a b)
;     (if (= a 0) b (+ (dec a) (inc b))))

; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
;; This procedure executes a linear iterative process.
;; All the information needed to compute the process is present in the values passed to the procedure
;; in each call, if the execution is initiated from any point the result will be the same.
;; Also, there's no expansion in the shape of the procedure execution (no deferred operations).

; Exercise 1.10: The following procedure computes a math-
; ematical function called Ackermann’s function.
(define (A x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (A (- x 1) (A x (- y 1))))))
; What are the values of the following expressions?
; (A 1 10)
; (A 2 4)
; (A 3 3)

; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; (A 0 (A 0 (A 0 (A 1 7))))
; (A 0 (A 0 (A 0 (A 0 (A 1 6)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 32)))))
; (A 0 (A 0 (A 0 (A 0 64))))
; (A 0 (A 0 (A 0 128)))
; (A 0 (A 0 256))
; (A 0 512)
; 1024

; (A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2))) 2 ^ (2 ^ 4)
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; (A 1 (A 0 (A 0 4)))
; (A 1 (A 0 8))
; (A 1 16)
; (A 0 (A 1 15))
; (A 0 (A 0 (A 1 14)))
; (A 0 (A 0 (A 0 (A 1 13))))
; (A 0 (A 0 (A 0 (A 0 (A 1 12)))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 9))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 8)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 7))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 6)))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 32)))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 64))))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 128)))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 256))))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 512)))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 1024))))))
; (A 0 (A 0 (A 0 (A 0 (A 0 2048)))))
; (A 0 (A 0 (A 0 (A 0 4096))))
; (A 0 (A 0 (A 0 8192)))
; (A 0 (A 0 16384))
; (A 0 32768)
; 65536

; (A 3 3)
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4) => This result is equal to 65536, as already calculated previously

(define (f n) (A 0 n)) ; => 2n
(define (g n) (A 1 n)) ; => 2^n
(define (h n) (A 2 n)) ; => 2 if n = 1; otherwise 2^2^2(n times), for example n = 5 -> 2^2^2^2^2
(define (k n) (* 5 n n)) ; => 5n²

; (A 2 1) => 2^1
; (A 2 2) => 2^2 -> 2^(2^1)
; (A 2 3) => 2^4 -> 2^(2^2)
; (A 2 4) => 2^16 -> 2^(2^4)
; (A 2 5) => 2^65536 -> 2^(2^16) -> 2^(2^(2^4)) -> 2^(2^(2^(2^2))) -> 2^(2^(2^(2^(2))))

;;; 1.2.2 Tree Recursion

;; Recursive Fibonacci
(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))

;; Iterative Fibonacci
(define (fib n)
    (fib-iter 1 0 n))
(define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

;; Example: Counting change
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount
                       (- kinds-of-coins 1))
                (cc (- amount
                       (first-denomination
                        kinds-of-coins))
                    kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))
;; TODO: Implement the iterative version of count-change

(define (a x)
  (display x))

(define (mult4 x)
  (* x 4))

(define (teste x) 
  (equal? x #t))
(define x 5)
(+ x x)

;; Testing the map function
(map cadr '((a b) (d e) (g h)))

(display "Testando 123")

;; Exercise 1.11:

;; Procedure using a recursive process
(define (f n)
  (if   (or (> n 3) (= n 3))
	(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))
	n))
;; Or:

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

;; Procedure using a iterative process
(define (f n)
  (f-iter 1 2 4 (- n 1)))
(define (f-iter a b c count)
  (if (= count 0)
      a
      (f-iter b c (+ c (* 2 b) (* 3 a)) (- count 1))))
;; Sequence: 1 2 4 11 25 59 142 335 796 ...

;; Or:

;; a = f(2) n times => f(n+2)
;; b = f(1) n times => f(n+1)
;; c = f(0) n times => f(n)
(define (f n)
  (f-iter 2 1 0 n))
(define (f-iter a b c count)
	(if (= count 0)
	  c
	  (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))
;; Sequence: 0 1 2 4 11 25 59 142 335 796 ...

;; Exercise 1.12:

(define (pascal line number)
  (if (or (= line 1) (= number 1) (= line number))
      1
      (+ (pascal (- line 1) (- number 1)) (pascal (- line 1) number))))

;; Displaying the numbers in each line from 1 up until the specified line:

(define (pascal-line line count)
  (if (= count 1)
      (begin
	(display " ")
	(display 1))
      (begin
	(display " ")
	(display (pascal line count))
	(pascal-line line (- count 1)))))

(define (pascal-triangle n initial-line)
  (if (> initial-line n)
      (display "")
      (begin
	(pascal-line initial-line initial-line)
	(display "\n")
	(pascal-triangle n (+ initial-line 1)))))

;; Exercise 1.13
;; TODO: Add image of the solution

;; Exercise 1.14
;; Number of steps equal to Fib(n-1) (number of nodes in the tree), we now know from exercise 1.13 that the value
;; of Fib(n) grows exponentially. So, the Order of Growth for the number of
;; steps is approx. O((p^n)/sqrt(5)), where p is the golden ratio.

;; The procedure produces a tree with a depth of 15, we can see that this number is probably a linear function of n.
;; So the order of growth for the order of growth for the space required is O(n)

;; Exercise 1.15
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

;; a. The procedure p is applied 5 times for (sine 12.15), as shown below:
;; (sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; (p (p (p (p .14950000000000002))))
;; (p (p (p .43513455050000005)))
;; (p (p .9758465331678772))
;; (p -.7895631144708228)
;; -.39980345741334

;; b. (sine a) generates a linear recursive procedure,
;; so the space and number of steps grow linearly with proportion to a -> O(a)

;; 1.2.4 Exponentiation

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; iterative version:
(define (expt b n)
  (expt-iter b n 1))
(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
		 (- counter 1)
		 (* b product))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
	((even? n) (square (fast-expt b (/ n 2))))
	(else (* b (fast-expt b (- n 1))))))
;; Exercise 1.16
;; First try (wrong solution):
;(define (fast-expt b n)
 ; (fast-expt-iter b n 1))
;(define (fast-expt-iter b counter product)
 ; (cond ((= counter 0) product)
;	((= counter 2) (if (= product 1)
;			   (fast-expt-iter b 0  (square b))
;			   (fast-expt-iter b 0 (square product))))
;	((even? counter) (fast-expt-iter b (/ counter 2) (* product (square b))))
;	(else (fast-expt-iter b (- counter 1) (* product b)))))

; Correct solution (source: https://sicp-solutions.net/post/sicp-solution-exercise-1-16/)
(define (fast-expt b n)
  (fast-expt-iter 1 b n))
(define (fast-expt-iter a b n)
  (cond ((= n 0) a)
	((even? n) (fast-expt-iter a (square b) (/ n 2)))
	(else (fast-expt-iter (* a b) b (- n 1)))))

;; Exercise 1.17
(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (fast-mult a b)
  (cond ((= b 0) 0)
	((even? b) (fast-mult (double a) (halve b)))
	(else (+ a (fast-mult a (- b 1))))))

;; Exercise 1.18
(define (fast-mult-iter a b n)
  (cond ((= n 0) a)
	((even? n) (fast-mult-iter a (double b) (halve n)))
	(else (fast-mult-iter (+ a b) b (- n 1)))))

;; Exercise 1.19
(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
	((even? count)
	 (fib-iter a
		   b
		   (+ (square p) (square q))		; compute p ′
		   (+ (* 2 p q) (square q))		; compute q ′
		   (/ count 2)))
	(else (fib-iter (+ (* b q) (* a q) (* a p))
			(+ (* b p) (* a q))
			p
			q
			(- count 1)))))

;; 1.2.5 Greatest Common Divisors

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; Exercise 1.20
;; Illustration of process for (gcd 206 40) using normal-order evaluation:
;; (gcd 206 40)
;; (if (= 40 0) 206 (gcd 40 (remainder 206 40)))

;; (gcd 40 (remainder 206 40))
;; (if (= (remainder 206 40) 0) 40 (gcd (remainder 206 40) (remainder 40 (remainder 206 40))))

;; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
;; (if (= (remainder 40 (remainder 206 40)) 0)
;;    (remainder 206 40)
;;    (gcd (remainder 40 (remainder 206 40))
;;	   (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))

;; (gcd (remainder 40 (remainder 206 40))
;;	   (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0)
;;     (remainder 40 (remainder 206 40))
;;     (gcd
;;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))))

;; (gcd
;;   (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;   (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
;; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0)
;;     (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;     (gcd
;;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;;      (remainder (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
;;		(remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))))

;; Q: How many remainder operations are actually performed in the normal-order evaluation of (gcd 206 40)?
;; A: the number of remainder operations performed in the normal-order evaluation of (gcd 206 40) is 18,
;; 14 times in the if's to check the logical value of the expression and 4 times for the consequent expression of the if.

;; Illustration of process for (gcd 206 40) using applicative-order evaluation:
;; (gcd 206 40)
;; (if (= 40 0)
;;     206
;;     (gcd 40 (remainder 206 40)))

;; (gcd 40 (remainder 206 40)) --> evaluates the arguments
;; (gcd 40 6)
;; (if (= 6 0)
;;     40
;;     (gcd 6 (remainder 40 6)))

;; (gcd 6 (remainder 40 6)) --> evaluates the arguments
;; (gcd 6 4)
;; (if (= 4 0)
;;     6
;;     (gcd 4 (remainder 6 4)))

;; (gcd 4 (remainder 6 4)) --> evaluates the arguments
;; (gcd 4 2)
;; (if (= 2 0)
;;     4
;;     (gcd 2 (remainder 4 2)))

;; (gcd 2 (remainder 4 2)) --> evaluates the arguments
;; (gcd 2 0)
;; (if (= 0 0)
;;     2
;;     (gcd 0 (remainder 2 0)))
;; 2

;; Q: In the applicative-order evaluation?
;; A: the number of remainder operations performed in the applicative-order evaluation of (gcd 206 40) is 4

;; 1.2.6 Example: Testing for Primality

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	  (square (expmod base (/ exp 2) m))
	  m))
	(else
	 (remainder
	  (* base (expmod base (- exp 1) m))
	  m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((fermat-test n) (fast-prime? n (- times 1)))
	(else false)))

;; Exercise 1.21
;; (smallest-divisor 199) ;Value: 199
;; (smallest-divisor 1999) ;Value: 1999
;; (smallest-divisor 19999) ;Value: 7

;; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; Some experiments:
(with-timings
           (lambda () (fib 35))
           (lambda (run-time gc-time real-time)
             (write (internal-time/ticks->seconds run-time))
             (write-char #\space)
             (write (internal-time/ticks->seconds gc-time))
             (write-char #\space)
             (write (internal-time/ticks->seconds real-time))
             (newline)))
(define (timed-fib-test n)
  (newline)
  (display n)
  (start-fib-test n (internal-time/ticks->seconds (real-time-clock))))
(define (start-fib-test n start-time)
  (if (> (fib n) 0)
      (report-prime (- (internal-time/ticks->seconds (real-time-clock)) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (report-time-for f n)
  (with-timings
           (lambda () (f n))
           (lambda (run-time gc-time real-time)
	     (display "Runtime: ")
	     (write (internal-time/ticks->seconds run-time))
             (write-char #\space)
             (write (internal-time/ticks->seconds gc-time))
             (write-char #\space)
             (write (internal-time/ticks->seconds real-time))
             (newline))))

(report-time-for prime? 10000019)
;; End of experiments

(define (search-for-primes start limit)
  (timed-prime-test start)
  (if (< start limit)
      (search-for-primes (+ start 2) limit)))

(search-for-primes 100000000000001 100000000000100)

;; * Since my computer is fast, and my scheme installation doesn't
;; have functions for registering milliseconds,
;; I had to start the experiments from 10000000000 instead of 1000

;; Results for three smallest primes larger than 10000000000:
;; 10000000019 -> .06999999999999995 seconds
;; 10000000033 -> .07000000000000006 seconds
;; 10000000061 -> .06999999999999995 seconds

;; Results for three smallest primes larger than 100000000000:
;; 100000000003 -> .21999999999999997 seconds
;; 100000000019 -> .21999999999999997 seconds
;; 100000000057 -> .20999999999999996 seconds

;; Results for three smallest primes larger than 1000000000000:
;; 1000000000039 -> .6500000000000004 seconds
;; 1000000000061 -> .6600000000000001 seconds
;; 1000000000063 -> .6399999999999997 seconds

;; Results for three smallest primes larger than 10000000000000:
;; 10000000000037 -> 2.039999999999999 seconds
;; 10000000000051 -> 2.0599999999999987 seconds
;; 10000000000099 -> 2.0600000000000023 seconds

;; Results for three smallest primes larger than 100000000000000:
;; 100000000000031 -> 6.489999999999998 seconds
;; 100000000000067 -> 6.490000000000002 seconds
;; 100000000000097 -> 6.529999999999994 seconds

;; Observing the results is possible to note that the time needed
;; to test each prime is indeed growing at approximately sqrt(10)

;; Q: Do your timing data bear this out?
;; A: Yes

;; Q: How well do the data for 100,000 and 1,000,000 support the Θ(√n) prediction?
;; A: Not well, the computation time in my machine is much faster than expected

;; Q: Is your result compatible with the notion
;; that programs on your machine run in time proportional to
;; the number of steps required for the computation?
;; A: Yes, even though the time needed for testing each prime is much smaller than
;; expected, when going from one order of magnitude to the next,
;; the time needed is approximately √10 times bigger.

;; Exercise 1.23

(define (next n)
  (if (> n 2)
      (+ n 2)
      3))
(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
	(else (find-divisor n (next test-divisor)))))
(define (divides? a b) (= (remainder b a) 0))

;; (search-for-primes 10000000001 10000000061)
;; Results (using the next procedure) for three smallest primes larger than 10000000000:
;; 10000000019 -> .06999999999999995 seconds (before using next), 3.9999999999999994e-2 seconds (after using next)
;; 10000000033 -> .07000000000000006 seconds (before using next), .05000000000000002 seconds (after using next)
;; 10000000061 -> .06999999999999995 seconds (before using next), .03999999999999998 seconds (after using next)

;; (search-for-primes 100000000001 100000000057)
;; Results (using the next procedure) for three smallest primes larger than 100000000000:
;; 100000000003 -> .21999999999999997 seconds (before using next), .12000000000000002 seconds (after using next)
;; 100000000019 -> .21999999999999997 seconds (before using next), .13999999999999996 seconds (after using next)
;; 100000000057 -> .20999999999999996 seconds (before using next), .13 seconds (after using next)

;; (search-for-primes 1000000000001 1000000000063)
;; Results (using the next procedure) for three smallest primes larger than 1000000000000:
;; 1000000000039 -> .6500000000000004 seconds (before using next), .41000000000000003 seconds (after using next)
;; 1000000000061 -> .6600000000000001 seconds (before using next), .4099999999999999 seconds (after using next)
;; 1000000000063 -> .6399999999999997 seconds (before using next), .40000000000000013 seconds (after using next)

;; (search-for-primes 10000000000001 10000000000099)
;; Results (using the next procedure) for three smallest primes larger than 10000000000000:
;; 10000000000037 -> 2.039999999999999 seconds (before using next), 1.25 seconds (after using next)
;; 10000000000051 -> 2.0599999999999987 seconds (before using next), 1.2600000000000002 seconds (after using next)
;; 10000000000099 -> 2.0600000000000023 seconds (before using next), 1.2400000000000002 seconds (after using next)

;; Q: Since this modiﬁcation halves the number of test steps, you should expect it to run about
;; twice as fast. Is this expectation conﬁrmed?
;; A: Yes, the tests show a reduction of roughly 50% in execution times.

;; Exercise 1.24
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

;; (search-for-primes 10000000001 10000000061)
;; Results (using the fast-prime? procedure) for three smallest primes larger than 10000000000:
;; 10000000019 -> .06999999999999995 seconds (before using fast-prime?), .05000000000000071 seconds (after using fast-prime?)
;; 10000000033 -> .07000000000000006 seconds (before using fast-prime?), .03999999999999915 seconds (after using fast-prime?)
;; 10000000061 -> .06999999999999995 seconds (before using fast-prime?), 4.0000000000000924e-2 seconds (after using fast-prime?)

;; (search-for-primes 100000000001 100000000057)
;; Results (using the fast-prime? procedure) for three smallest primes larger than 100000000000:
;; 100000000003 -> .21999999999999997 seconds (before using fast-prime?), .03999999999999915 seconds (after using fast-prime?)
;; 100000000019 -> .21999999999999997 seconds (before using fast-prime?), 4.0000000000000924e-2 seconds (after using fast-prime?)
;; 100000000057 -> .20999999999999996 seconds (before using fast-prime?), 4.9999999999998934e-2 seconds (after using fast-prime?)

;; (search-for-primes 1000000000001 1000000000063)
;; Results (using the fast-prime? procedure) for three smallest primes larger than 1000000000000:
;; 1000000000039 -> .6500000000000004 seconds (before using fast-prime?), .05000000000000071 seconds (after using fast-prime?)
;; 1000000000061 -> .6600000000000001 seconds (before using fast-prime?), .05000000000000071 seconds (after using fast-prime?)
;; 1000000000063 -> .6399999999999997 seconds (before using fast-prime?), 4.9999999999998934e-2 seconds (after using fast-prime?)

;; (search-for-primes 10000000000001 10000000000099)
;; Results (using the fast-prime? procedure) for three smallest primes larger than 10000000000000:
;; 10000000000037 -> 2.039999999999999 seconds (before using fast-prime?), .05000000000000071 seconds (after using fast-prime?)
;; 10000000000051 -> 2.0599999999999987 seconds (before using fast-prime?), 4.9999999999998934e-2 seconds (after using fast-prime?)
;; 10000000000099 -> 2.0600000000000023 seconds (before using fast-prime?), .05000000000000071 seconds (after using fast-prime?)

;; Q: Since the Fermat test has Θ(log n) growth, how would you expect
;; the time to test primes near 1,000,000 to compare with the time needed to test primes near 1000?
;; A: I would expect to take the same time, since the number of tests in the fast-prime? call
;; inside timed-prime-test is fixed.
;; Q: Do your data bear this out?
;; A: Yes.

;; Exercise 1.25
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (remainder
	  (square (expmod base (/ exp 2) m))
	  m))
	(else
	 (remainder
	  (* base (expmod base (- exp 1) m))
	  m))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

;; Q: Is she correct?
;; A: Yes.
;; Q: Would this procedure serve as well for our fast prime tester? Explain.
;; A: Yes, partially. The procedure generate large numbers when calling fast-expt
;; with a big enough base or exp, since the original expmod performs the remainder
;; operation with the m value, the intermediate return values when computing the final return value
;; will be way smaller requiring less computational power to perform the operations. This factor makes
;; the original expmod implementation more suitable to work with large numbers.

;; Exercise 1.26
;; By using two calls to expmod to calculate the square for even exponents
;; Louis' code is defeating the purpose of the original implementation, because his code is
;; using two calls that halve the input size, basically the code is doing the calculation in linear time.

;; Exercise 1.27
(define (foolable-fermat-test n)
  (define (iter a n)
    (cond ((= a n) true)
	  ((= (expmod a n n) a) (iter (+ a 1) n))
	  (else false)))
  (iter 1 n))

;; The above procedure returns #t (true) to the following
;; Carmichael numbers: 561, 1105, 1729, 2465, 2821, and 6601

;; Exercise 1.28

;; Iterative expmod for an experiment
(define (expmod-iter a base exp m)
  (cond ((= exp 0) a)
	((even? exp) (expmod-iter a (remainder (square base) m) (/ exp 2) m))
	(else
	 (expmod-iter (remainder (* a base) m) base (- exp 1) m))))

;; Inspiration: http://community.schemewiki.org/?sicp-ex-1.28
(define (expmod base exp m)
  (cond ((= exp 0) 1)
	((even? exp)
	 (nontrivial-square-test (expmod base (/ exp 2) m) m))
	(else
	 (remainder
	  (* base (expmod base (- exp 1) m))
	  m))))

(define (nontrivial-square-test x m)
  (let ((y (remainder (square x) m)))
    (if (and (= y 1) (not (= x 1)) (not (= x (- m 1))))
	0
	y)))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
	((miller-rabin-test n) (fast-prime? n (- times 1)))
	(else false)))

;; TODO: Study mutual recursion to really understand the solution

;; 1.3 Formulating Abstractions with Higher-Order Procedures

;; 1.3.1 Procedures as Arguments
 (define (sum-integers a b)
   (if (> a b)
       0
       (+ a (sum-integers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a)
	 (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2)))
	 (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
	 (sum term (next a) next b))))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01) ;; => .24998750000000042
(integral cube 0 1 0.001) ;; => .249999875000001

;; Exercise 1.29

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y n) (f (+ a (* n h))))
  (define (integral-term x)
    (if (even? x)
	(* 2 (y x))
	(* 4 (y x))))
  (* (/ h 3) (+ (y 0) (y n) (sum integral-term 1 inc (- n 1)))))

(simpson-integral cube 0 1 100) ;; => 1/4
(simpson-integral cube 0 1 1000) ;; => 1/4

;; Exercise 1.30
(define (sum term a next b)
  (define (iter a result)
  (if (> a b)
    result
    (iter (next a) (+ (term a) result))))
  (iter a 0))

;; Exercise 1.31

;; a)
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
	 (product term (next a) next b))))

;; Factorial defined in terms of product:
(define (factorial n)
  (product identity 1 inc n))
(factorial 6) ;; => 720
(factorial 7) ;; => 5040

;; Approximated value of pi/2 using Wallis product:
;; https://en.wikipedia.org/wiki/Wallis_product
(define (pi-wallis n)
  (define (wallis-term x)
    (* (/ (* 2 x) (- (* 2 x) 1))
       (/ (* 2 x) (+ (* 2 x) 1))))
  (product wallis-term 1 inc n))

;; b) Iterative version of product
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* (term a) result))))
  (iter a 1))

;; Exercise 1.32
;; a.
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate combiner null-value term (next a) next b))))

;; Defining sum and product in terms of accumulate
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;; b. Iterative version of accumulate
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (combiner (term a) result))))
  (iter a null-value))

;; Exercise 1.33
(define (filtered-accumulate predicate? combiner null-value term a next b)
  (cond
   ((> a b) null-value)
   ((predicate? a) (combiner (term a)
			     (filtered-accumulate predicate?
						  combiner
						  null-value
						  term (next a) next b)))
   (else
    (filtered-accumulate predicate?
			 combiner
			 null-value
			 term (next a) next b))))

;; Testing the procedure, this should return 6, 2+4
(filtered-accumulate even? + 0 identity 1 inc 4)

;; a.
(filtered-accumulate prime? + 0 square 1 inc 10)

;; b.
;; Choose n as 10 for the answer
(define (relatively-prime? i)
  (= 1 (gcd i 10)))
(filtered-accumulate relatively-prime? * 1 identity 1 inc 10)

;; 1.3.2 Constructing Procedures Using lambda
(lambda (x) (+ x 4))
(lambda (x) (/ 1.0 (* x (+ x 2))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
     a
     (lambda (x) (+ x 4))
     b))
(define (integral f a b dx)
  (* (sum f
	  (+ a (/ dx 2.0))
	  (lambda (x) (+ x dx))
	  b)
     dx))

;; Using let to create local variables
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
	    (- 1 y)))

(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
	(* y b)
	(* a b)))
   (+ 1 (* x y))
   (- 1 y)))

(define (f x y)
  (let ((a (+ 1 (* x y)))
	(b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; Exercise 1.34
(define (f g) (g 2))
(f square) ;; => 4
(f (lambda (z) (* z (+ z 1)))) ;; => 6

;; Q: What happens if we (perversely) ask the interpreter to
;; evaluate the combination (f f)? Explain.
;; A: The call to (f f) would return (f 2), since the parameter passed to
;; f will be called as a function with the number 2, the result will be
;; the list (2 2), 2 is not a function and the interpreter will throw
;; an error.
