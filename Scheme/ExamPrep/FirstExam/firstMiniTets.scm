#lang racket

(define (authomorphic? n)
  (define (iter n squared)
    (cond
      [(= n 0) #t]
      [(not (= (remainder n 10) (remainder squared 10))) #f]
      [else
       (iter (quotient n 10) (quotient squared 10))
       ]
      )
    )
  (iter (abs n) (* n n))
  )

(define (get-distance p1 p2)
  (sqrt
   (+
    (expt (- (car p1) (car p2)) 2)
    (expt (- (cdr p1) (cdr p2)) 2)
    )
   )
  )
(define (get-min-distance arr)
  (define (iter cur arr res)
    (if (null? arr) res
        (iter
         (car arr)
         (cdr arr)
         (apply min res (map (lambda (y) (get-distance y cur )) arr))
                  )
          )
         )
        
  (iter (car arr) (cdr arr) (get-distance (car arr) (cadr arr)))
  )





