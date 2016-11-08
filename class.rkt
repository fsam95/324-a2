#lang racket
#| Assignment 2 - Classes

This file contains your work for Questions 1 and 2, extending the basic
class macro to include support for traits and some basic introspection.
|#
(provide class-meta class-trait)

; QUESTION 1 (metaprogramming).
(define-syntax class-meta
  (syntax-rules ()
    [(class-meta <Class> 
       (<attr> ...)
       [(<method> <param> ...) <body>] 
       ...)
     (define (<Class> <attr> ...)
       (lambda (msg) 
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [(equal? msg (id->string _attributes))
                (sort (list (list (id->string <attr>) <attr>) ...) #:key car string<?)]
               [(equal? msg (id->string _methods))
                (sort (list (list (id->string <method>) 
                            (lambda (<param> ...) <body>)) ...) #:key car string<?)]
               [else "Unrecognized message!"]))
    )]))


; QUESTION 2 (traits).
(define-syntax class-trait
  (syntax-rules ()
    [(class-trait <Class> 
       (<attr> ...) (with)
       [(<method> <param> ...) <body>] 
       ...)
     (class <Class> (<attr> ...) [(<method> <param> ...) <body>] ...)] 
    
    [(class-trait <Class> 
       (<attr> ...) (with <trait>)
       [(<method> <param> ...) <body>] 
       ...)
     (define (<Class> <attr> ...)
       (let ([obj 
         (lambda (msg)
           (cond [(equal? msg (id->string <attr>)) <attr>]
                 ...
                 [(equal? msg (id->string <method>))
                  (lambda (<param> ...) <body>)]
                 ...
                 [else "Unrecognized message!"]))]) 
         (<trait> obj)))
     ]
    
     #|
    [(class-trait <Class> 
       (<attr> ...) (with <trait1> <trait2> ...)
       [(<method> <param> ...) <body>]
       ...)
     (define (<Class> <attr> ...)
       (let ([obj 
         (lambda (msg)
           (cond [(equal? msg (id->string <attr>)) <attr>]
                 ...
                 [(equal? msg (id->string <method>))
                  (lambda (<param> ...) <body>)]
                 ...
                 [else "Unrecognized message!"]))])
         (<trait1> (class-trait (<attr> ...) (with <trait2> ...) [(<method> <param> ...) <body>] ...))))
     (void)
     ]
     |#
    ))



; -----------------------------------------------------------------------------
; Class macro. This section is just for your reference.
; -----------------------------------------------------------------------------
(define-syntax class
  (syntax-rules ()
    [(class <Class> (<attr> ...)
       [(<method> <param> ...) <body>] ...)
     (define (<Class> <attr> ...)
       (lambda (msg)
         (cond [(equal? msg (id->string <attr>)) <attr>]
               ...
               [(equal? msg (id->string <method>))
                (lambda (<param> ...) <body>)]
               ...
               [else "Unrecognized message!"]))
       )]))

(define-syntax id->string
  (syntax-rules ()
    [(id->string <id>)
     (symbol->string (quote <id>))]))
