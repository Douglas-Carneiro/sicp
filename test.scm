(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(define x 10)

(define (factorial n)
  (if (= n 1)
    1
    (* n (factorial (- n 1)))))

(define (fact n)
  (define (iter m i)
    (cond ((> i n) m)
      (else (iter (* i m) (+ i 1)))))
  (iter 1 1))

(define make-counter 
  (lambda (n) 
      (lambda () 
        (set! n (1+ n))
        n)))

(define c1 (make-counter 0))
(define c2 (make-counter 10))

;Start the interpreter with the command: "rlwrap -r -c -f /home/douglas/Documentos/SICP/mit-scheme-bindings.txt scheme" for accessing command history and tab completion

; Use (load "test.scm") to load the file in the REPL

; Just use the command: scm-cmpt to use the scheme interpreter with rlwrap enabled. (If not using Emacs)
