#| Assignment 2 - Using Backtracking

This file contains starter code for questions 4-6,
which involve using backtracking in interesting ways, and
extending the functionality of the backtracking library.
|#
#lang racket

; Import choice API
(require "choice.rkt")

; Export functions for testing. Please don't change this line!
(provide subsets sudoku-4 fold-<)

; QUESTION 3
#|
(subsets lst)
  lst: a list

  A choice expression which yields a subset of 'lst'.
  Repeated calls to 'next' should yield *all* other subsets
  (i.e., 'next' returns "false." only when all subsets of 'lst'
  have been returned).

  The subsets can be yielded in any order; however, no subset
  can appear twice.

  Note that:
    - A subset isn't the same as a sublist. Items don't have to be consecutive.
    - A subset can be empty; the empty set is a subset of any list.
    - Order doesn't matter in subsets
  
  The following is an example of how 'subsets' is used.
  Note that your implementation might yield the subsets
  in a different order than the one shown here.

> (subsets '(1 2))
'()
> (next)
'(1)
> (next)
'(2)
> (next)
'(1 2)
> (next)
"false."
|#
(define (subsets lst)
  (if (empty? lst)
      '()
      (append (-< '() (list (first lst))) (subsets (rest lst)))))


; QUESTION 4
#|
(sudoku-4 puzzle)
  puzzle: a nested list representing a 4-by-4 Sudoku puzzle

  A choice expression that represents possible solutions to the puzzle.
  Upon evaluation, just one solution is returned, but repeated calls
  to 'next' produces more possible solutions, if any.

  Hint: use the ?- function in your solution. Again, your main task
  is just to correctly express the constraints, and let the computer
  do the work.
|#
(define (sudoku-4 puzzle)
  (?- solved? (fill-in-choices puzzle)))

(define (solved? puzzle)
  (let* ([cols (get-cols puzzle '())]
        [quarters (get-quarters puzzle '())])
    (and (solved-subgrids? puzzle) (solved-subgrids? cols) (solved-subgrids? quarters))))

(define (solved-subgrids? subgrids)
  (if (empty? subgrids)
      #t
      (if (not (solved-subgrid? (first subgrids)))
          #f
          (solved-subgrids? (rest subgrids)))))

(define (solved-subgrid? subgrid)
  (if (check-duplicates subgrid) #f #t))

(define (get-cols puzzle acc)
  (let* ([trimmed-puzzle (filter-empty-lists puzzle)])
    (if (empty? trimmed-puzzle)
        acc
        (get-cols (map (lambda (x) (list-tail x 1)) trimmed-puzzle)
                  (append acc (list (map first trimmed-puzzle)))))))

(define (get-quarters puzzle acc)
  (let* ([trimmed-puzzle (filter-empty-lists puzzle)])
     (if (empty? trimmed-puzzle)
      acc
      (let* ([rows (take trimmed-puzzle 2)]
             [unmodified-rows (list-tail trimmed-puzzle 2)]
             [square (apply append 
                            (map (lambda (x) (take x 2)) rows))]
             [truncated-rows (map (lambda (x) (list-tail x 2)) rows)]
             [truncated-puzzle (append truncated-rows unmodified-rows)])
        (get-quarters truncated-puzzle (append acc (list square)))))))

(define (fill-in-choices puzzle)
  (map replace-empty-cells-in-row-with-choices puzzle))

(define (replace-empty-cells-in-row-with-choices row)
  (map substitute-empty-cell-with-choices row))

(define (substitute-empty-cell-with-choices cell)
  (if (equal? cell "")
      (-< 1 2 3 4)
      cell))

(define (filter-empty-lists lst)
  (filter non-empty-list? lst))

(define (non-empty-list? lst)
  (not (empty? lst)))


; QUESTION 5
#|
(fold-< combine init expr)
  combine: a binary function
  init: an initial value
  expr: a choice expression

  Evaluate all choices in <expr> and combine them, one at a time, with the
  initial value, and return the result.

  Note that the order of <combine>'s parameters is the same as foldl:
    1) The value of the next choice
    2) The value of <init>
|#
(define-syntax fold-<
  (syntax-rules ()
    [(fold-< <combine> <init> <expr>)
     (foldl <combine> <init> (all <expr>))]
    ))