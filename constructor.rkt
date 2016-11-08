#lang racket
(define-syntax class-with-constructor
  (syntax-rules ()
    [(class-with-constructor <Class> (<constructor-lambda> ...) (<attr> ...)
      [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) (<constructor-lambda> (list <attr> ...))]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(class-with-constructor MyClass ((lambda(attrs)(f (list-ref attrs 0))) (lambda(attrs)(list (list-ref attrs 1) 100 (f (list-ref attrs 0)))) (lambda(attrs)(substring "you are cool" 0))) (x y z))

(define (f r)
  (+ r 5))

(class-with-constructor Point-plus-one ((lambda(attrs)(+ (list-ref attrs 0) 1)) (lambda(attrs)(+ (list-ref attrs 1) 1))) (x y))

;works fine
(define p1 (Point-plus-one 1 2))
;(p1 "x")
;(p1 "y")

(define m1 (MyClass 1 "a" (void)))
;(m1 "x")
;(m1 "y")
;(m1 "z")
#|
Would be better if we had 
(class-with-constructor MyClass ((lambda(attrs)(f x)) (lambda(attrs)(y 100 (f x))) (lambda(attrs)("some const"))))

Would be even better if we had
(class-with-constructor MyClass ((f x)(list y 100 z) "some const") (x y z))

|#

(define-syntax cwc
  (syntax-rules ()
    [(cwc <Class> (<val1> ...) (<attr> ...)
          [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <val1>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(cwc MyClass2 ((f x) (list y 100 (f x)) "some const") (x y z))
(define m2 (MyClass2 1 "a" (void)))

(m2 "x")
(m2 "y")
(m2 "z")

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]
    ))

