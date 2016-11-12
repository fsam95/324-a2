#lang racket

#|
(class-with-constructor <Class> (<val1> ...) (<attr> ...)
  [(method <param> ...) <body>] ...])
  ...

Similar to the original class macro except it takes modifiers for the 
parameters in the form (<val1> ...). <val1> will consist of either:
  - function(s) applied to parameter <attr1> 
  OR
  - a constant 
    - if a constant is supplied, <attr1> should be (void)
      so that to imply that <val1> will be a constant

> (define (f r) (+ r 5))
> (class-with-constructor MyClass ((f x) (list y 100 (f x)) "some const") (x y z))
> (define m (MyClass 1 "a" (void)))
> (m "x")
6 
> (m "y")
'("a" 100 6)
> (m "z")
"some const"
|#
(define-syntax class-with-constructor
  (syntax-rules ()
    [(class-with-constructor <Class> (<val1> ...) (<attr> ...)
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
(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]
    ))

(define (f r)
  (+ r 5))

#| 
MyClass implementation
|#
(class-with-constructor MyClass ((f x) (list y 100 (f x)) "some const") (x y z)
     [(addntox n)(+ x n)])

(define m (MyClass 1 "a" (void)))

(m "x")
(m "y")
(m "z")
((m "addntox") 4)

