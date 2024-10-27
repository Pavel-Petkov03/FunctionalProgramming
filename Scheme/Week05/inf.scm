#lang scheme
(define (assoc-rec key xs)
  (cond
    [(null? xs) #f]
    [(= (car (car xs)) key) (cdr (car xs))]
    [else
     (assoc-rec key (cdr xs))
     ]
    )
  
  )

(define (assoc-hop el xs)
  (foldr (lambda (cur res) (or (if (= (car cur) el) (cdr cur) #f ) res)) #f xs)
  )


(define (assoc-assoc el xs)
  (let
      (
       (res (assoc el xs))
       )
    (if (not res) res (cdr res))
      )
  )

(define (assoc-del el xs)
  (let
      (
       (filtered (filter (lambda (x) (not (= (car x) el))) xs))
       )
      
       (if (= (length filtered) (length xs)) xs
           filtered
           )
       
      )
  )

(define (replace xs assoc-xs)
     (cond [(null? xs) xs]
           [(not (assoc-rec (car xs) assoc-xs)) (cons (car xs) (replace (cdr xs) assoc-xs))]
           [else
            (cons (assoc-rec (car xs) assoc-xs) (replace (cdr xs) assoc-xs))]
    ))




(define (make-f-list xs func)
  (foldr (lambda (x res) (cons (cons x (func x)) res)) '() xs)
  )

(define (assoc-keys assoc-arr)
  (foldr (lambda (x rest) (cons (car x) rest)) '() assoc-arr)
  )
(define (assoc-values assoc-arr)
  (foldr (lambda (x rest) (cons (cdr x) rest)) '() assoc-arr)
  )

(define (zero-rows xss)
  (map (lambda (current-row)
         (if ( = (length current-row)
                 (length (filter (lambda (current-col)
                                   (not (= current-col 0))) current-row)
             ))
             current-row
             (map (lambda (dummy) 0) current-row)
         )
   ) xss)
  )




