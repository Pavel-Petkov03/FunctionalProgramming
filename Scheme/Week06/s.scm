#lang racket
(define (foldr op init l)
  (if (null? l)
      init
      (op (car l) (foldr op  init (cdr l)))))

(define (foldl op init l)
  (if (null? l)
      init
      (foldl op (op init (car l)) (cdr l))))



(define (sum xs)
  (foldr (lambda (x y) (+ x y)) 0 xs)
  )

(define (len xs)
  (foldr (lambda (x y) (+ y 1)) 0 xs)
  )

(define (filter pred?  xs)
  (foldr (lambda (x r) (if (pred? x) (cons x r) r)) '() xs)
  )

(define (map f  xs)
  (foldr (lambda (x r) (cons (f x) r)) '() xs)
  )

(define (all? pred? xs)
  (foldr (lambda (x y) (and (pred? x) y)) #t xs)
  )

(define (any? pred? xs)
  (foldr (lambda (x y) (or (pred? x) y)) #f xs)
  )


(define (foldr* op l)
  (foldr op (car l) (cdr l))
  )

(define (min-max-build* pred ls)
  (foldr* (lambda (x y) (if (pred x y) y x)
            )
          ls
          )
  )

(define (minimum xs)
  (min-max-build* < xs)
  )
(define (maximum xs)
  (min-max-build* > xs)
  )

(define (reverse ls)
  (foldl (lambda (x y ) (cons y x)) '() ls)
  )

(define (take n l)
  (if (= n 0)
      '()
      (cons (car l) (take (- n 1) (cdr l)) )
      )
  )
(define (drop n l)
  (if (= n 0)
      l
      (drop (- n 1) (cdr l))
      )
  )
(define (take-while pred? xs)
  (if (not (pred? (car xs)))
      '()
      (cons (car xs) (take-while pred? (cdr xs)))
  ))


(define (drop-while pred? xs)
  (if (not (pred? (car xs)))
      xs
      (drop-while pred? (cdr xs))
  ))

(define (zip l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))
  ))

(define (zip-with f l1 l2)
  (if (or (null? l1) (null? l2))
      '()
      (cons (f (car l1) (car l2)) (zip-with f (cdr l1) (cdr l2)))
  )
  )


