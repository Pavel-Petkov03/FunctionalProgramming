#lang racket
(define (accumulate op nv a b term next)
  (if (> a b) nv
          (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
          (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (filter p l)
  (cond ((null? l) l)
        ((p (car l)) (cons (car l) (filter p (cdr l))))
        (else (filter p (cdr l)))))

(define (foldr op nv l)
  (if (null? l) nv
      (op (car l) (foldr op nv (cdr l)))))

(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))))

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
      (op (car l) (foldr1 op (cdr l)))))

(define (foldl1 op l)
  (foldl op (car l) (cdr l)))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

(define (map-stream f . streams)
  (cons-stream (apply            f (map head streams))
               (apply map-stream f (map tail streams))))

(define (filter-stream p? s)
  (if (p? (head s))
      (cons-stream (head s) (filter-stream p? (tail s)))
                            (filter-stream p? (tail s))))


(define (is-atom? xss)
  (and (not (list? xss)) (not (null? xss))))

(define (deep-map-cond-recurse xss p f1 f2 current-height)
  (cond
    [(and (is-atom? xss) (p xss current-height)) (f1 xss current-height)]
    [(and (is-atom? xss) (not (p xss current-height))) (f2 xss current-height)]
    [(null? xss) xss]
    [else
     (append
      (list (deep-map-cond-recurse (car xss) p f1 f2 (+ 1 current-height)))
      (deep-map-cond-recurse (cdr xss) p f1 f2 current-height))
     ]
  ))
(define (deel-map-cond xss p f1 f2)
  (deep-map-cond-recurse xss p f1 f2 0)
  )

(deel-map-cond `(1 (2 (5 1 ) 4) 3) > (lambda (x d ) d) (lambda (x d) (* x 2)))

