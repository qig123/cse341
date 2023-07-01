
#lang racket

;; This line exports all of your defined functions,
;; so you can call them in hw4tests.rkt and so we can
;; call them in our tests.
;; Don't remove or comment out this line!
(provide (all-defined-out)) 

;; Implement your code below

;; #1
(define (sequence spacing low high)
  (cond [(<= low high) (cons low (sequence spacing (+ low spacing) high))]
        [#t '()]))
;; keep going with the rest of the problems below
;; #2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))
;; #3
(define (list-nth-mod xs n)
  (cond
    [(< n 0) error "list-nth-mod:negative number"]
    [(null? xs) error "list-nth-mod:empty list"]
    [#t ( car (list-tail xs (remainder n (length xs))))]))
;; #4
(define (stream-first-k-such-that f k s)
  (define (iter count k s)
    (cond [(= count k) '()]
          [#t (cons (car (s)) (iter (+ count 1) k (cdr (s))))]))
  (iter 0 k s))
;; #5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons (g x)(lambda () (f (+ x 1)))))]
           [g (lambda (x) (if (= 0 (remainder x 6)) (- x) x))])
    (lambda () (f 1))))
;; #6
(define dan-then-dog
  (letrec ([f (lambda (x) (cons (g x)(lambda () (f (+ x 1)))))]
           [g (lambda (x) (if (= 0 (remainder x 2)) "dog.jpg" "dan.jpg"))])
    (lambda () (f 1))))
;; #7
(define (stream-add-one s)
  (letrec ([f (lambda (x) (cons (iter 0 x s)(lambda () (f (+ x 1)))))]
           [iter (lambda (n x s) (if (= n x) ( cons 1 (car (s))) (iter (+ n 1) x (cdr (s)))))])
    (lambda () (f 0))))
;; #8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons (iter x xs ys)(lambda () (f (+ x 1)))))]
           [iter (lambda (n xs ys) (cons (list-nth-mod xs n) (list-nth-mod ys n)))])
    (lambda () (f 0))))
;; #9
(define (vector-assoc v vec)
  (define (iter pos)
    (if (>= pos (vector-length vec))
        #f
        (let ((current-item (vector-ref vec pos)))
          (if (pair? current-item)
              (if (equal? (car current-item) v)
                  current-item
                  (iter (+ pos 1)))
              (iter (+ pos 1))))))
  (iter 0))
;; #10
(define (caching-assoc xs n)
   (letrec ([cache (make-vector n)]
            [current-pos 0]
            [f (lambda (v)
                 (let ([ans (vector-assoc v cache)])
                   (if (pair? ans) (begin (display "exist value ") (cdr ans) )
                       (let ([new-ans (assoc v xs)])  
                         (begin (vector-set! cache current-pos (cons v new-ans) )
                              (set! current-pos (if (= (+ current-pos 1 ) n) 0 (+ current-pos 1)))
                              (display "new value "  )
                              new-ans
                              )))))])
     f))
(define test10f (caching-assoc (list '(1 2) '(2 3) '(3 4)) 2))
(define test101 (equal? (test10f 1) (assoc 1 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test101 = ") (displayln test101))
(define test102 (equal? (test10f 2) (assoc 2 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test102 = ") (displayln test102))


(define test103 (equal? (test10f 1) (assoc 1 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test103 = ") (displayln test103))


(define test104 (equal? (test10f 3) (assoc 3 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test104 = ") (displayln test104))
(define test105 (equal? (test10f 4) (assoc 4 (list '(1 2) '(2 3) '(3 4)))))
(begin (display "test105 = ") (displayln test105))



  
  






