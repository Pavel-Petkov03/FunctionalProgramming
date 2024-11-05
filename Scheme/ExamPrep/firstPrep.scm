#lang racket


(define (sum-all-dels n)
  (foldr (lambda (x res) (if (= (remainder n x) 0) (+ res x) res)) 0 (range 1 n))
  )

(define (done? n)
  (= (sum-all-dels n) (+ n 2))
  )

(define (any f ls)
  (foldr (lambda (cur res) (or (f cur) res)) #f ls)
  )

(define (sum-almost-done a b)
  (let
      (
       [done-lst (filter done? (range a b))]
       )
    (foldr + 0 (filter (lambda (x) (any (lambda (cur-done)
                             (< (abs (- x cur-done)) (min (- x a) (- b x)))) done-lst
                             )
                          
            ) (range a (+ 1 b)))
      )
  
  ))

(define (run-operation ls op times)
  (if (or (= times 0) (null? (car ls)) (null? (cadr ls)) (symbol? (car ls)) (symbol? (cadr ls)))
      ls
      (run-operation (cons (op (car ls) (cadr ls)) (cddr ls))
                     op
                     (- times 1))
      )
  )

(define (run-machine ls)
  (define (wrapper ls res)
    (cond
        [(null? ls) res]
        [(or (symbol? (car ls)) (number? (car ls))) (wrapper (cdr ls) (cons (car ls) res))]
        [(pair? (car ls)) (wrapper (cdr ls) (run-operation res (caar ls) (cdr (car ls))))]
        [else 
          (wrapper (cdr ls) (map (lambda (x) (if (number? x) ((car ls) x) x)) res))
         ]
        
    )
    )
  (wrapper ls '())
  )


(define (major? l1 l2)
  (define (iter l1 l2)
    (if (null? l1) #t
        (and (<= (car l1) (car l2)) (major? (cdr l1) (cdr l2)))
        )
    )
  (if (not (= (length l1) (length l2)))
           #f
           (iter l1 l2)
           )
  )

(define (is-major? ls)
  (if (or (null? ls) (null? (cdr ls)))
          #t
          (and 
           (any
            (lambda (arr) (major? (car ls) arr))
            (generate-all-sub-arr (cadr ls) (length (car ls)) (length (cadr ls)))
            )
           (is-major? (cdr ls))
           )
          )
  )

(define (generate-all-sub-arr arr n arr-len)
  (if (< arr-len n)
      '()
      (cons (take arr n) (generate-all-sub-arr (cdr arr) n (- arr-len 1)))
      )
  )


(define (longest-major ls)
  (define (iter ls res)
    (if (or (null? ls) (null? (cdr ls)))
          res
          (iter (cdr ls) (foldr (lambda (x r)
                   (if (and (major? (car ls) x) (> (length x) (length r)))
                       x
                       r
                   ))
                 '()
                 (generate-all-sub-arr (cadr ls) (length (car ls)) (length (cadr ls)))
          ))
    )
  )
  (iter ls '())
  )

