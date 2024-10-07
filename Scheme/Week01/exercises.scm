#lang scheme

;(1)
(define (square var) (* var var))
(+ (/(+ 3 5) 2) (sqrt (- (square 4) (* 7 (square 2)))))
(/ (+ (+ 5 1/4) (- 2 (- 3 (+ 6 1/5)))) (* 3 (* (- 6 2) (- 2 7))))
(/ (+ 15 21 (/ 3 15) (- 7 (* 2 2))) 16)


;(2)
(define (my-not x) (if x #f #t))
(define (my-and x y) (if x (if y #t #f) #f))
(define (my-or x y) (if x #t (if y #t #f)))
(define (my-xor x y) (if (my-and(my-not(my-and x y)) (my-or x y)) #t #f))

;(3)
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))
      )
  )

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))
       )
 )

(define (count-digits number)
  (define (count-digits-wrapper number accumulator)
    (if (< number 10) (+ accumulator 1) (count-digits-wrapper (/ number 10) (+ accumulator 1))))
  (count-digits-wrapper number 0)
  )

(define (reminder first second)
  (begin (define int-div (quotient first second)) (define res (* int-div second)) (- first res ))
 )

(define (convert-from-base-10-to-base-k base-k n)
  (if (= n 0) "" (string-append (convert-from-base-10-to-base-k base-k (floor (/ n base-k))) (number->string (reminder n base-k)))) ; backa ;)
  )
(define (is-palindrome? str)
  (begin (define len (string-length str))
         (cond
           ((= len 1) #t)
           ((= len 0) #t)
           (else (and  (= (char->integer(string-ref str 0))
                     (char->integer(string-ref str (- (string-length str) 1)))
                   )
                 (is-palindrome? (substring str 1 (- len 1)))
                 
            ))
           )
        )
  )



(define (iterative-fact n)
  (define (iteratrive-fact-wrapper n acc)
    (if (= n 0) acc (iteratrive-fact-wrapper (- n 1) (* acc n)))
   )
  (iteratrive-fact-wrapper n 1)
)

(define (iterative-fib n)
  (define (iterative-fib-wrapper n acc)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+( iterative-fib-wrapper (- n 1) acc)
                  (iterative-fib-wrapper (- n 2) acc)
                  )
                )
          )
    )
  (iterative-fib-wrapper n 0)
  )



