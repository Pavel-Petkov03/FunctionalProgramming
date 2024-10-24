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
  (if (= n 0)
      lst
      (takeRight (cdr lst) (- n 1))
      )
  )

(define (takeLeft lst n)
  (if (= n 0)
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))
      )
  )

(define (merge-sort lst n pred?)
  (if (<= n 1)
      lst
      (let*
          (
           {mid (quotient n 2)}
           {leftArr (merge-sort (takeLeft lst mid) mid)}
           {rightArr (merge-sort (takeRight lst (- n mid)) (- n mid))}
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





