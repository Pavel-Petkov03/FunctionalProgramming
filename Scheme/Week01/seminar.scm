(define (custom-fact n)
  (if (< n 0)
      "Cannot find fact with negative number"
      (if (= n 0)
      1
      (* n (custom-fact (- n 1)))
       )
      )
  )
 
(define add +)


(define (count-digits n)
  (define (count-digits-recurse n iter)
    (if (= n 0)
        iter
        (count-digits-recurse (quotient n 10) (+ iter 1)) ; tail rec
        )
    )
  (if (= n 0)
      1
      (count-digits n 0)
      )
  )


(define (list-sum arr)
  (define (list-sum-inner arr iter)
    (if (null? arr)
        iter
        (list-sum-inner (cdr arr) (+ iter (car arr))) ; cdr -> takes everything except for the first element
                                                      ; car -> takes the first element
        )
    )
  (list-sum-inner arr 0)
  )