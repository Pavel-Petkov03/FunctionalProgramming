#lang scheme
(define (merge left right pred?)
  (cond
    [(null? left) right]
    [(null? right) left]
    [(pred? (car left) (car right)) (cons (car left)
                                      (merge (cdr left) right pred?)
                                      )]
    [else
     (cons (car right) (merge left (cdr right) pred?))
     ]
   )
  )

(define (takeRight lst n) ; приемаме че хората са коректни и подават големината на масива
  (if (or (= n 0) (null? lst))
      lst
      (takeRight (cdr lst) (- n 1))
      )
  )

(define (takeLeft lst n)
  (if (or (= n 0) (null? lst))
      lst
      (cons (car lst) (take (cdr lst) (- n 1)))
      
  ))

(define (merge-sort lst n pred?)
  (if (<= n 1)
      lst
      (let*
          (
           {mid (quotient n 2)}
           {leftArr (merge-sort (takeLeft lst mid) mid pred?)}
           {rightArr (merge-sort (takeRight lst (- n mid)) (- n mid) pred?)}
           )
        (merge leftArr rightArr pred?)
      )
     )
); ne moga da povqrvam che tva trugna ot putrviq put


(define len
  (lambda (ls)
    (if (null? ls)
        0
        (+ 1 (len (cdr ls)))
        )
    )
  ) ; ползва stack ама ме мързи да пиша iter-wrapper 

(define (sort-list ls) ; това го правя защото това искат в задача 1
                        ; да се върне функция от по висок ред която приема comparer
                        ; и сортира спрямо него
  (lambda (pred?)
         (merge-sort ls (len ls) pred?)
         )
  )

;----

(define (get-smallest list)
  (define (recursor list current-smallest)
    (cond
      [(null? list) current-smallest]
      [(< (car list) current-smallest) (recursor ( cdr list) (car list))]
      [else
        (recursor (cdr list) current-smallest)
       ]
      )
  )
  (recursor list (car list))
  )

(define (remove-first ls element)
  (define (remove-first-recurse ls is-found)
    (cond
      [(null? ls) '()]
      [(and (equal? element (car ls)) (not is-found)) (remove-first-recurse (cdr ls) #t)]
      [else
       (cons (car ls) (remove-first-recurse (cdr ls) is-found))
       ]
      )
    )
  (remove-first-recurse ls #f)
  )

; remove all e the same ama без is-found

(define (num-to-xs number)
  (if (= number 0)
      '()
      (append (num-to-xs (quotient number 10)) (list (remainder number 10))) ; важно!!!!
                                                                    ; ако имаме (cons vale reslist)
      )
  )

; важно!!!!
; ако имаме (cons value reslist) -> list
; ако имаме (cons reslist value) -> linked-list(dotted pairs linked)
; ako искаме build на list добавяне в ляво трябва да ползваме append


(define (xs-to-num ls)
  (define (xs-to-num-iter ls length)
    (if (null? ls)
        0
        (+ (* (expt 10 (- length 1))
              (car ls)
              )
           (xs-to-num-iter (cdr ls)
                           (- length 1))
           )
        )
    )
  (xs-to-num-iter ls (len ls))
  )




(define (list-to-ordered-set ls)
  (define (list-to-ordered-set-recurse ordered)
    (cond
      [(null? ordered) ordered]
      [(and (not (null? (cdr ordered)))
            (= (car ordered) (car (cdr ordered)))
            )
       (list-to-ordered-set-recurse (cdr ordered))
       ]
      [else
       (cons (car ordered) (list-to-ordered-set-recurse (cdr ordered)))
       ]
      )
    )
  (list-to-ordered-set-recurse (merge-sort ls (length ls) <))
  ); тази функция не е в задачите просто ми беше интересно колко бързо ще стане



(define (kth-max-negative ls)
    (lambda (x)
       (list-ref (merge-sort ls (length ls) <) x)
      )
  )

(define (from-to a b); правя го inclusive
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))
      )
  )

(define (prime-arr-gen num)
  (filter (lambda (x) (= (remainder num x) 0 )) (from-to 2 (floor num)))
  ); трябва да измлисля начин за lazy map като в python защото това прави 2n повторения

(define (factorise num)
  (foldr (reverse (from-to 2 (floor (sqrt num)))) 
  )
