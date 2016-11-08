#lang racket

#|
User enters a list of lambdas;
one for each attribute.

|#
(define-syntax class-with-constructor
  (syntax-rules ()
    [(class-with-constructor <Class> (<constructor-lambda> ...) (<attr> ...)
      [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) (<constructor-lambda> <attr>)]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))
#|
Can this be used to write 

class MyClass:
  def __init__(self, a, b):
    r = f(a)
    self.x = f(a)
    self.y = [b, 100, r]
    self.z = 'you are cool'

def f(r):
  return r + 5

gonna need three lambdas
|#

(define (f r)
  (+ r 5))
(class-with-constructor MyClass ((lambda(a)(f a)) (lambda(y)(list b 100 (f a))) 



(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]
    ))

(class-with-constructor Point-plus-one ((lambda(x)(+ x 1)) (lambda(y)(+ y 1))) (x y))

;works fine
(define p1 (Point-plus-one 1 2))
(p1 "x")
