#lang racket
(define (insert-at x position xs)
  (append (take xs position ) (list x) (drop xs position))
  )



(define (get-sublist start-idx end-idx xs)
  (take (drop xs start-idx) (- end-idx 1))
  )

(define (count-occurrences arr xs)
  (define (wrapper arr xs len current-len)
    (if (null? xs) 0
        (+
            (if (and (>= current-len len) (equal? arr (take xs len))) 1 0
        )
       (wrapper arr (cdr xs) len (- current-len 1))
       )
        )
    )
  (wrapper arr xs (length arr) (length xs))
  )


(define (ordered? xs)
  (lambda (pred?)
    (cond [(or (null? xs) (null? (cdr xs))) #t]
          [(not (pred? (car xs) (cadr xs))) #f]
          [else
           ((ordered? (cdr xs)) pred?) ; връщам lambda тоест трябва да я викна (това прави доста среди което е кофти)
           ]
      )
    )
  )
(define (where xs preds)
  (cond
    [(null? xs) xs]
    [(equal? (filter (lambda (current-pred) (current-pred (car xs))) preds) preds) (cons (car xs) (where (cdr xs) preds))]
    [else
     (where (cdr xs) preds)
     ]
    )
  )

(define (my-cartesian-product l r)
  (define (inner current-l current-r)
    (cond [(null? current-l) current-l]
          [(null? current-r) (inner (cdr current-l) r)]
          [else
           (cons (cons (car current-l) (car current-r)) (inner current-l (cdr current-r)))
           ]
      )
    )
  (inner l r)
  )

(define (atom? x)
  (and (not (null? x)) (not (pair? x)))
  )

(define (my-flatten dl)
  (cond
    [(null? dl) dl]
    [(atom? (car dl)) (cons (car dl) (my-flatten (cdr dl)))]
    [else
     (append (my-flatten (car dl)) (my-flatten (cdr dl)))
     ]
    )
  )
(define (range a b)
    (if (> a b) '()
        (cons a (range (+ a 1) b))
        )
  )

(define (tabulate f)
  (lambda (a b)
    (map (lambda (x) (cons x (f x))) (range a b))
  ))

(define (deep-delete xs)
  (define (deep-del-recurse dl depth)
    (cond
      [(null? dl) dl]
      [(list? (car dl)) (cons (deep-del-recurse (car dl) (+ 1 depth)) (deep-del-recurse (cdr dl) depth))]
      [(>= (car dl) depth) (cons (car dl) (deep-del-recurse (cdr dl) depth))]
      [else
       (deep-del-recurse (cdr dl) depth)
       ]
      )
    )
  (deep-del-recurse xs 1)
  )

(define (shuffle xs)
  (define (wrapper l r lrem?)
    (cond
      [(and (null? l) (null? r)) '()]
      [lrem? (cons (car l) (wrapper (cdr l) r (not lrem?)))]
      [else
       (cons (car r) (wrapper l (cdr r) (not lrem?)))
       ]
      )
    )
  (let
      (
      (half-len (/ (length xs) 2)))
    (wrapper (take xs half-len) (drop xs half-len) #t)
    
      )
  )


    
    
    
    
