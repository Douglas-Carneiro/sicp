;;; Exercise 1.1

10 ; => ;Value: 10

(+ 5 3 4) ; ;Value: 12

(- 9 1) ;Value: 8

(/ 6 2) ;Value 3

(+ (* 2 4) (- 4 6)) ;Value: 6

(define a 3) ;Value: a

(define b (+ a 1)) ;Value: b

(+ a b (* a b)) ;Value: 19

(= a b) ;Value: #f

(if (and (> b a) (< b (* a b)))
    b
    a) ;Value: 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;Value: 16

(+ 2 (if (> b a) b a)) ;Value: 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
    (+ a 1)) ;Value: 16
    
;;; Exercise 1.2
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))

;;; Exercise 1.3
(define (square x) (* x x))

(define (sum-of-squares x y) (+ (square x) (square y)))

(define (sum-of-squares-largest-two a b c)
    (if (> a b)
        (if (> b c) 
            (sum-of-squares a b)
            (sum-of-squares a c))
        (if (> b c)
            (sum-of-squares a b)
            (sum-of-squares b c))))

;;; Exercise 1.4
(define (a-plus-abs-b a b)
    ((if (> b 0) + -) a b))
; If the value of b is bigger than zero the if special form will return the + (sum) procedure, 
; so the a-plus-abs-b procedure will return the sum of arguments a and b.
; If the value of b is not bigger than zero the if special form will return the - (subtraction) procedure and the 
; a-plus-abs-b procedure will return the result of a minus b.

;;; Exercise 1.5
;(define (p) (p))
;(define (test x y)
;    (if (= x 0) 0 y))
;(test 0 (p))

; If the code above is executed on an interpreter that uses aplicative order evaluation a infinite execution will
; start, since p is defined in terms of itself and p is evaluated in the call to to the procedure test.
; If the code above is executed on an interpreter that uses normal order evaluation the result will be zero,
; since the execution of the if the condition will be true and zero is returned,
; because the value of p is never needed in the call to test it is never going to be evaluated and executed.