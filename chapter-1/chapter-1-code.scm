
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
