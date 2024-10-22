#lang scheme
(define (merge left right)
  (cond
    [(null? left) right]
    [(null? right) left]
    [(<= (car left) (car right)) (cons (car left)
                                      (merge (cdr left) right)
                                      )]
    [else
     (cons (car right) (merge left (cdr right)))
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

(define (merge-sort lst n)
  (if (<= n 1)
      lst
      (let*
          (
           {mid (quotient n 2)}
           {leftArr (merge-sort (takeLeft lst mid) mid)}
           {rightArr (merge-sort (takeRight lst (- n mid)) (- n mid))}
           )
        (merge leftArr rightArr)
      )
     )
); ne moga da povqrvam che tva trugna ot putrviq put


