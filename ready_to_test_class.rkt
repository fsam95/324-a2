#lang plai
(require "class.rkt")
(class-meta Point-meta (x y)
  [(distance other-point)
   (let ([dx (- x (other-point "x"))]
         [dy (- y (other-point "y"))])
     (sqrt (+ (* dx dx) (* dy dy))))])
     
(test (let ([p (Point-meta 2 3)])
        (p "_attributes"))
      '(("x" 2)
        ("y" 3)))


(test (let ([p (Point-meta 2 3)])
        (first (first (p "_methods"))))
      "distance")


