#lang racket

(require "hw4.rkt") 

;; A simple library for displaying a 2x3 grid of pictures: used
;; for fun in the tests below (look for "Tests Start Here").

(require (lib "graphics.rkt" "graphics"))
(require rackunit)

(open-graphics)

(define window-name "Programming Languages, Homework 4")
(define window-width 700)
(define window-height 500)
(define border-size 100)

(define approx-pic-width 200)
(define approx-pic-height 200)
(define pic-grid-width 3)
(define pic-grid-height 2)

(define (open-window)
  (open-viewport window-name window-width window-height))

(define (grid-posn-to-posn grid-posn)
  (when (>= grid-posn (* pic-grid-height pic-grid-width))
    (error "picture grid does not have that many positions"))
  (let ([row (quotient grid-posn pic-grid-width)]
        [col (remainder grid-posn pic-grid-width)])
    (make-posn (+ border-size (* approx-pic-width col))
               (+ border-size (* approx-pic-height row)))))

(define (place-picture window filename grid-posn)
  (let ([posn (grid-posn-to-posn grid-posn)])
    ((clear-solid-rectangle window) posn approx-pic-width approx-pic-height)
    ((draw-pixmap window) filename posn)))

(define (place-repeatedly window pause stream n)
  (when (> n 0)
    (let* ([next (stream)]
           [filename (cdar next)]
           [grid-posn (caar next)]
           [stream (cdr next)])
      (place-picture window filename grid-posn)
      (sleep pause)
      (place-repeatedly window pause stream (- n 1)))))
;; Tests Start Here

; These definitions will work only after you do some of the problems
; so you need to comment them out until you are ready.
; Add more tests as appropriate, of course.

(define nums (sequence 1 0 5))

(define files (string-append-map 
               (list "dan" "dog" "curry" "dog2") 
               ".jpg"))

(define funny-test (stream-first-k-such-that (lambda (x) #t) 16 funny-number-stream))
(define funny-test1 (stream-first-k-such-that (lambda (x) #t) 16 dan-then-dog ))
(define funny-test2 (stream-first-k-such-that (lambda (x) #t) 5 (stream-add-one dan-then-dog) ))

; a zero-argument function: call (one-visual-test) to open the graphics window, etc.
(define (one-visual-test)
  (place-repeatedly (open-window) 0.5 (cycle-lists nums files) 27))
(one-visual-test)
; similar to previous but uses only two files and one position on the grid
(define (visual-one-only)
  (place-repeatedly (open-window) 0.5 (stream-add-one dan-then-dog) 27))
(visual-one-only)

(define tests
  (test-suite
   "Sample tests for Assignment 4"
   
   ; sequence test
   (check-equal? (sequence 1 0 5) (list 0 1 2 3 4 5) "Sequence test")

   ; string-append-map test
   (check-equal? (string-append-map 
                  (list "dan" "dog" "curry" "dog2") 
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")
   ; list-nth-mod test
   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")
   ; funny-number-stream test
   (check-equal? (stream-first-k-such-that (lambda (x) #t) 18 funny-number-stream) (list 1 2 3 4 5 -6 7 8 9 10 11 -12 13 14 15 16 17 -18) "funny-number-stream test")
   (check-equal? (stream-first-k-such-that (lambda (x) #t) 5 dan-then-dog )
                      '("dan.jpg" "dog.jpg" "dan.jpg" "dog.jpg" "dan.jpg"))
   (check-equal? (stream-first-k-such-that (lambda (x) #t) 5 (stream-add-one dan-then-dog)) 
                      '((1 . "dan.jpg") (1 . "dog.jpg") (1 . "dan.jpg") (1 . "dog.jpg") (1 . "dan.jpg")) )
   (check-equal?
               (stream-first-k-such-that  (lambda (x) #t) 7 (cycle-lists '(1 2 3) '("a" "b")) )
               '((1 . "a") (2 . "b") (3 . "a") (1 . "b") (2 . "a") (3 . "b") (1 . "a")))
   
   (check-equal? (vector-assoc 3 (vector '(1 2) 3 '(2 3))) #f)
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
