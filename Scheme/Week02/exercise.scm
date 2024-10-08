#lang scheme

(define (reminder first second)
  (begin (define int-div (quotient first second)) (define res (* int-div second)) (- first res ))
 ) ; this will be copied because I am too lazy to write it every time

(define (sum-digits-recurse number) ; uses stack (no tail recursion)
  (if (= number 0)
      0
      (+ (reminder number 10) (sum-digits-recurse (floor (/ number 10))))
    
      )
  )

(define (sum-digits-iter number)
  (define (sum-digits-wrapper number accumulator)
    (if (= number 0) accumulator (sum-digits-wrapper (floor (/ number 10)) (+ accumulator (reminder number 10))))
    )
  (sum-digits-wrapper number 0)
  )


(define (count-devisors n)
  (define (count-devisors-iter n current accumulator)
    (if (= current 0)
        accumulator
        (count-devisors-iter n
                             (- current 1)
                             (if (= (reminder n current) 0 )
                                 (+ accumulator 1)
                                 accumulator
                                 )
                             )
        )
    )
  (count-devisors-iter n n 0) ; bih optimiziral do koren ama iskat i 1 i n
  )


(define (isPrime? num) (= (count-devisors num) 2))

