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
  (count-devisors-iter n n 0)
  )


(define (tup-prime? num) (= (count-devisors num) 2)) ; ako si baven se pishe tova (1 ne e prime)

(define ( num)
  (define (umen-prime-inner number iter root)
    (if (> iter root)
        #t
        (if ( = (reminder number iter) 0 )
            #f
            (umen-prime-inner number (+ iter 1) root)
        )
     )
    )
  (umen-prime-inner num 2 (sqrt num))
  )

(define (get-number-len number)
  (define (get-number-len-wrapper number)
  (if (= number 0)
      0
      (+ 1 (get-number-len-wrapper (quotient number 10)))
      )
  )
  (if (= number 0)
      1
      (get-number-len-wrapper number)
      )
)





(define (increasing-digits? number)
  (increasing-digits-wrapper number 9) ; 9 zashoto e nai golqmata cifra
  )
(define (increasing-digits-wrapper number current-max-digit)
  (cond
    ((<= current-max-digit (remainder number 10)) #f)
    ((= number 0) #t)
    (else 
     (increasing-digits-wrapper (quotient number 10) (remainder number 10)))
    )
  ) ;; tui kato sme umni se seshtame da gledame decreasing otdqsno nalqvo zashtoto e the same ;)


(define (ends-with-inner? n k)
  (if (or (= n 0) (= k 0))
      #t
      (and (= (remainder n 10) (remainder k 10))
           (ends-with-inner? (quotient n 10) (quotient k 10))
           )
      )
  )



(define (ends-with? n k)
  (cond ((and (= n 0)(= k 0))  #t)
        ((or (= n 0) (= k 0) #f))
        (else
         (ends-with-inner? n k)
         )
      
  )
)



