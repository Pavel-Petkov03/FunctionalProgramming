#lang scheme

(define (partial-fraction n)
  
  )



(define (partial-fraction-wrapper current gen accumulator n)
  (cond
    [(= n (+ current gen)) (+ accumulator 1)]
    [(> (+ current gen) n) accumulator]
    [
     ]
    )
  )





