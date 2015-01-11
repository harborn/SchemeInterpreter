
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; quotation
’"abc" ; "abc"
’145932 ; 145932
’a ; a
’#(a b c) ; #(a b c)
’() ; ()
’(+ 1 2) ; (+ 1 2)
’(quote a) ; (quote a)
’’a ; (quote a)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Base types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

boolean?
pair?
symbol?
number?
char?
string?
vector?
procedure?
null?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;; quotation
(quote a) ; a
(quote #(a b c)) ; #(a b c)
(quote (+ 1 2)) ; (+ 1 2)
`"abc" ; "abc"
`145932 ; 145932
`a ; a
`#(a b c) ; #(a b c)
`() ; ()
`(+ 1 2) ; (+ 1 2)
`(quote a) ; (quote a)
``a ; (quote a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Procedures ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (lambda <formals> <body>)

(lambda (x) (+ x x)) ; a procedure
((lambda (x) (+ x x)) 4) ; 8
((lambda (x) (define (p y) (+ y 1)) (+ (p x) x)) 5) ; 11
(define reverse-substract (lambda (x y) (- y x))) ; a procedure
(reverse-substract 7 10) ; 3
(define add4 (let ((x 4)) (lambda (y) (+ x y)))) ; a procedure
; expand as
(define add4 ((lambda (x) (lambda (y) (+ x y))) 4))
(add4 6) ; 10



((lambda x x) 3 4 5 6) ; (3 4 5 6)
((lambda (x y . z) z) 3 4 5 6) ; (5 6)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Conditionals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; if 
;(if <test> <consequent> <alternate>)
;(if <test> <consequent>)

(if (> 3 2) `yes `no) ; yes
(if (> 2 3) `yes `no) ; no
(if (> 3 2) (- 3 2) (+ 3 2)) ; 1
(if #f #f) ; unspecified

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Assignments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! <variable> <expression>)
(let ((x 2)) (+ x 1) (set! x 4) (+ x 1)) ; 5


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; when 
(when (< (pressure tube) 60)
    (open-valve tube)
    (attach floor-pump tube)
    (depress floor-pump 5)
    (detach floor-pump tube)
    (close-valve tube))
    
(if (< pressure tube> 60)
    (begin
        (open-valve tube)
        (attach floor-pump tube)
        (depress floor-pump 5)
        (detach floor-pump tube)
        (close-valve tube)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; unless 

(unless (>= pressure tube) 60)
    (open-valve tube)
    (attach floor-pump tube)
    (depress floor-pump 5)
    (detach floor-pump tube)
    (close-valve tube))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; cond

(cond ((> 3 2) `greater) ((< 3 2) `less)) ; greater
(cond ((> 3 3) `greater) ((< 3 3) `less) (else `equal)) ; equal

; (cond <cond clause1> <cond clause2> ...)
; <cond clause> -> (<test> <expression>)

(cond ((= 3 3) `equal1) ((= 4 4) `equal2))
(define (my-sum lst) (cond ((empty? lst) 0) ((list? (car lst)) (+ (my-sum (car lst)) (my-sum (cdr lst)))) (else (+ (car lst) (my-sum (cdr lst))))))

(define (empty? lst) (if (= (length lst) 0) (list? `()) (list? 3)))

(define (my-sum lst) 
    (cond 
        ((empty? lst) 0) 
        ((list? (car lst)) (+ (my-sum (car lst)) (my-sum (cdr lst)))) 
        (else (+ (car lst) (my-sum (cdr lst))))))      
; expand =>
(define my-sum 
    (lambda (lst) 
        (if (empty? lst) 
            0 
            (if (list? (car lst)) (+ (my-sum (car lst)) (my-sum (cdr lst))) 
            (+ (car lst) (my-sum (cdr lst)))))))
            
(define my-sum (lambda (lst) (if (empty? lst) 0 (if (list? (car lst)) (+ (my-sum (car lst)) (my-sum (cdr lst))) (+ (car lst) (my-sum (cdr lst)))))))            

(my-sum '((1) (2 3) (4) (5 6)))
(my-sum '((1) (2 3) (4) (5 6 (7 8 (9)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; case

(case c ((#\a) 1) ((#\b) 2) ((#\c) 3) (else 4))
(case (* 2 3) ((2 3 5 6 7) 'prime) ((1 4 6 8 9) 'composite))

(case (* 2 3) ((2 3 5 7) `prime) ((1 4 6 8 9) `composite)) ; composite
(case (car `(c d)) ((a) `a) ((b) `b)) ; unspecified
(case (car `(c d)) (( a e i o u) `vowel) ((w y) `semivowel) (else `consonant)) ; consonant

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; and

(and (= 2 2) (> 2 1)) ; #t
(and (= 2 2) (< 2 1)) ; #f
(and 1 2 `c `(f g)) ; (f g)
(and) ; #t

(define-syntax and
    (syntax-rules ()
        ((and) #t)
        ((and test) test)
        ((and test1 test2 ...)
         (if test1 (and test2 ...) #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; or

(or 1 2) ; 1
(or #f 1) ; 1
(or (= 2 2) (> 2 1)) ; #t
(or (= 2 2) (< 2 1)) ; #t
(or #f #f #f) ; f
(or `(b c) (/ 3 0)) ; (b c)

(define-syntax or
    (syntax-rules ()
        ((or) #f)
        ((or test) test)
        ((or test1 test2 ...)
         (let ((x test1))
          (if x x (or test2 ...))))))
         

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Binding constructs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; let
(let ((x 2) (y 3)) (* x y)) ; 6
; expand as ((lambda (x y) (* x y)) 2 3 )
(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) ; 35
; expand as ((lambda (x y) ((lambda (x z) (* z x)) 7 (+ x y ))) 2 3)
(define add4 (let ((x 4)) (lambda (y) (+ x y))))
; expand as (define add4 ((lambda (x) (lambda (y) (+ x y))) 4))

(define add4 (let ((x 4)) (lambda (y) (+ x y))))       ;  =>     (define add4 ((lambda (x) (lambda (y) (+ x y))) 4))
(let ((x 2) (y 3)) (* x y))   ;   =>     ((lambda (x y) (* x y)) 2 3)
(let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x)))  ;  =>     ((lambda (x y) ((lambda (x z) (* z x)) 7 (+ x y))) 2 3)
