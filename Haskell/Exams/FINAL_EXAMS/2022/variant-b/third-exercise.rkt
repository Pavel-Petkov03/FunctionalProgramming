#lang racket


(define (get-filled-array actual-nums desired-length)
  (define (iter nums desired-length)
    (cond
      [(null? nums) (get-filled-array actual-nums desired-length)]
      [(= desired-length 0) '()]
      [else
       (cons (car nums) (iter (cdr nums) (- desired-length 1)))
       ]
  ))
  (iter actual-nums desired-length)
  )

(define (find-max funcs nums)
  (define desired-length ( + 1 (length funcs)))
  (define (iter funcs nums)
    (if (null? (cdr funcs))
        ((car funcs) (car nums) (cadr nums))
        ((car funcs) (car nums) (iter (cdr funcs) (cdr nums)))
    )
  )
  (define (outer-iter nums times)
    (if (= times 0)
        '()
        (cons (iter funcs nums) (outer-iter (append (cdr nums) (list (car nums))) (- times 1)))
    ))
  (apply max (outer-iter (get-filled-array nums desired-length) (length nums)))
)
 ;; ппц е вярно ама това (append (cdr nums) (list (car nums))) е алгоритмично престъпление
;; нямам deque, така че така го оставям
