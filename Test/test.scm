
(define x 3)

(+ 2 2)
(+ (* 2 100) (* 1 10))
(if (> 6 5) (+ 1 1) (+ 2 2))
(if (< 6 5) (+ 1 1) (+ 2 2))
((lambda (x) (+ x x)) 5) ; func

(define twice (lambda (x) (* 2 x))) ; func
(define compose (lambda (f g) (lambda (x) (f (g x))))) ; func
(define repeat (lambda (f) (compose f f))) ; func

((compose list twice) 5)
((repeat twice) 5)
((repeat (repeat twice)) 5)

(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1)))))) ; func
(define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) ; func
(fact 3)
(fact 50)

(define fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))) ; func
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))) ; func
(define (fib n) (define (fib-kernal n1 n2 f1 f2) (if (< n1 n2) (fib-kernal (+ n1 1) n2 (+ f1 f2) f1) f2)) (fib-kernal 0 n 1 0)) ; func

(define add-const (lambda (y) (begin (define x 1) (set! x (+ x 1)) (+ x y)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (fib n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
; expanded as
(define fib (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2))))))

(define (fib n) (define (fib-kernal n1 n2 f1 f2) (if (< n1 n2) (fib-kernal (+ n1 1) n2 (+ f1 f2) f1) f2)) (fib-kernal 0 n 1 0))
; expanded as
(define fib (lambda (n) (begin (define fib-kernal (lambda (n1 n2 f1 f2) (if (< n1 n2) (fib-kernal (+ n1 1) n2 (+ f1 f2 ) f1) f2))) (fib-kernal 0 n 1 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add x y) (+ x y))
; expanded as
(define add (lambda (x y) (+ x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (double4 x) (define (double2 x) (+ x x)) (+ (double2 x) (double2 x)))
;==>
(define double4 (lambda (x) ((define (double2 x) (+ x x)) (+ (double2 x) (double2 x)))))

(define double4 (lambda (x) (define (double2 x) (+ x x))))

; internal expression, by python code
(define double4 (lambda (x) (define (double2 x) (+ x x)) (+ (double2 x) (double2 x))))


; internal expression, by c++ code
(define double4 (lambda (x) (define (double2 x) (+ x x)) (+ (double2 x) (double2 x))))
(define double2 (lambda (x) (+ x x)))


; expanded expression, by python code
(define double4 (lambda (x) (begin (define double2 (lambda (x) (+ x x))) (+ (double2 x) (double2 x)))))

; expanded exression, by c++ code
(define double4 (lambda (x) (begin (define double2 (lambda (x) ((+ x x)))) (+ (double2 x) (double2 x)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;set! 
(define greeted null)
(define (greet name) (set! greeted (cons name greeted)) (string-append "Hello, " name))
(greet "Athos")
(greet "Porthos")
(greet "Aramis")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fringe

(define x (list (list 1 2) (list 3 4))) 
(if (list? (car x)) (cons (car x) (cdr x)))

(define (fringe tree) (if (null? tree) '() (if (not (pair? tree)) (cons tree '()) (cons (car tree) (fringe (cdr tree))))))

(define (fringe tree) 
    (if (null? tree) 
        '() 
        (if (not (pair? tree))
            (cons tree '()) 
            (cons (car tree) (fringe (cdr tree))))))


(define my-tree (list 1 (list 2 (list 3 4) (list 5 6)) (list 7 (list 8))))
(define (fringe tree) (define nil '()) (if (null? tree) nil (let ((first (car tree))) (if (not (pair? first)) (cons first (fringe (cdr tree))) (cons (fringe first) (fringe (cdr tree)))))))


(define (fringe tree) 
    (define nil '()) 
    (define (build-fringe x result) 
        (cond 
            ((null? x) result) 
            ((not (pair? x)) (cons x result)) 
            (else (build-fringe (car x) (build-fringe (cdr x) result))))) 
    (build-fringe tree nil)) 

(define (fringe tree) (define nil '()) (define (fringe-kernal x result) (if (null? x) result (if (not (pair? x)) (cons x result) (fringe-kernal (car x) (fringe-kernal (cdr x) result))))) (fringe-kernal tree nil))
    
(define (fringe tree) (define (fringe-kernal x result) (if (null? x) result (if (not (pair? x)) (cons x result) (fringe-kernal (car x) (fringe-kernal (cdr x) result))))) (fringe-kernal tree '()))
    
(define (fringe tree)
    (define (fringe-kernal x result)
        (if (null? x) 
            result 
            (if (not (pair? x)) 
                (cons x result) 
                (fringe-kernal (car x) (fringe-kernal (cdr x) result)))))
    (fringe-kernal tree '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define insertL-f (lambda (test?) (lambda (new old l) (cond ((null? l) '()) ((test? (car l) old) (cons new (cons old (cdr l)))) (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertL-f (lambda (test) (lambda (new old l) (cond ((null? l) '()) ((test (car l) old) (cons new (cons old (cdr l)))) (else (cons (car l) ((insertL-f test) new old (cdr l))))))))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else
          (cons (car l) ((insertL-f test?) new old (cdr l))))))))
          
((insertL-f eq?) 'd 'e '(a b c e f g d h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; call/cc, Call-with-current-continuation
(call/cc (lambda (throw) (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (escape 3))))))))
; 35, (+ 5 (* 10 3)) -> 35
; 
(call/cc (lambda (throw) (+ 5 (* 10 (call/cc (lambda (escape) (* 100 (throw 3))))))))
; 3, (






















