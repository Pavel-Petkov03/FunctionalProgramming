#lang scheme
(define (abs number)
  (if (>= number 0) number (- number)) ;; absolute value function
 )


(define (get-day-by-number number)
  (cond ((= number 1) "Monday")
         ((= number 2) "Tuesday")
         ((= number 3) "Wednesday")
         ((= number 4) "Thursday")
         ((= number 5) "Friday")
         ((= number 6) "Saturday")
         ((= number 7) "Sunday")
    )
  )

#|
Exercise 1.3: Define a procedure that takes three numbers
as arguments and returns the sum of the squares of the two
larger numbers.
|#
(define (min a b) (if (< a b) a b))
(define (max a b) (if (> a b) a b))
(define (square x) (* x x))
(define (sum-squares x y) (+ (square x) (square y)))
(define (get-max-number first second third )
  (begin (define firstSum (sum-squares first second))
         (define secondSum (sum-squares second third))
         (define thirdSum (sum-squares first third))
         (max (max firstSum secondSum) thirdSum) ;; I think there is better solution but this is for now :)
   )
  )



