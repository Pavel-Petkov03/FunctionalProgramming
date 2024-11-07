#lang racket
(define (alist-operation alist key value op)
  (map (lambda (x)
         (if (equal? key (car x))
             (cons (car x) ( op (cdr (assoc (car x) alist)) value))
             x
             )
         )alist)
  )

(define (add-assoc-or-accumulate alist key value op)
  (if (assoc key alist)
      (alist-operation alist key value op)
      (cons (cons key value) alist)
  ))


(define (count-points-dict tournament)
  (foldr (lambda (x res)
           (cond
             ((= (caddr x) (cadddr x)) (add-assoc-or-accumulate (add-assoc-or-accumulate
                                                                 res (cadr x) 1 +
                                                                 )
                                                                (car x)
                                                                1
                                                                +
                                                                ))
             ((> (caddr x) (cadddr x)) (add-assoc-or-accumulate res (car x ) 3 +))
             (else
              (add-assoc-or-accumulate res (cadr x ) 3 +)
              )
                 
             )
           )
         '()
         tournament
         ))

(define (count-goals-made tournament)
  (foldr
   (lambda (x res)
     (add-assoc-or-accumulate (add-assoc-or-accumulate res (car x) (caddr x) +) (cadr x) (cadddr x) +)
     )
   '()
   tournament
   ))

(define (count-goals-taken tournament)
  (foldr
   (lambda (x res)
     (add-assoc-or-accumulate (add-assoc-or-accumulate res (car x) (cadddr x) +) (cadr x) (caddr x) +)
     )
   '()
   tournament
   ))

(define tournament '(("A" "B" 1 0)
                     ("B" "C" 4 1)
                     ("C" "B" 3 3)
                     ("B" "A" 1 2)
                     ("A" "C" 0 1)
                     ))


(define (max-dif  win-dict lost-dict)
  (foldr
   (lambda (x m)
    (max m (- (cdr x) (cdr (assoc (car x) lost-dict)) ))
     )
   (- (cdar win-dict) (cdar lost-dict))
   win-dict
   )
  )

(define (filter-maxed win-dict lost-dict dif)
  (filter (lambda (x)(= dif (- (cdr x) (cdr (assoc (car x) lost-dict))))) win-dict)
)
  
(define (max-goal-min-points tournament)
  (let*
      (
       [goals-taken-dict (count-goals-taken tournament)]
       [goals-made-dict (count-goals-made tournament)]
       [mdif  (max-dif goals-made-dict goals-taken-dict)]
       [filtered-maxed (map car (filter-maxed goals-made-dict goals-taken-dict mdif))]
       [points-dict (count-points-dict tournament)]
       [filtered-points-dict (filter (lambda (x) (member (car x) filtered-maxed)) points-dict)]
       )
    (if (= (length filtered-points-dict) 1)
        (caar filtered-points-dict)
        (car (foldr (lambda (x res)
                 (< (cdr x) (cdr res)) x res
                 )
               (car filtered-points-dict)
               filtered-points-dict
               ))))
  )

