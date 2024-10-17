#lang scheme
; car -> left
; cdr -> right



(define (len arr)
  (define (arr-iter arr acc)
    (if (null? arr)
        acc
        (arr-iter (cdr arr) (+ acc 1))
        )
    )
  (arr-iter arr 0)
  )