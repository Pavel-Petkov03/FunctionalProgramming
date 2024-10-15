#lang scheme
; car -> left
; cdr -> right

(define len
  (lambda (arr accumulator)
    (len (cdr arr) (+ accumulator (car arr)))
    )
  )