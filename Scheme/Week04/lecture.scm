#lang scheme
(define (list? ls)
  (if (null? ls) #t
      (and (pair? ls) (list? (cdr ls))))
  )


;(cadr l) = (car (cdr l))
;(cddr l) = (cdr (cdr l) )
;caddr = (car (cdr (cdr l)))
;може се нества така до 4

(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))
      )
  )

(define (append first second)
  (if (null? first) second
      (cons (car first) (append (cdr first) second))
      )
  )

(define (snoc x l); 0(n)
  (append l (list x))
  ) ; reverse cons

(define (reverse l1); 0(n^2) -> за по - добър performance foldl
  (if (null? l1) l1
      (snoc (car l1) (reverse (cdr l1)))
      )
  )
(define (reverse2 l1); пак 0(n^2)
  (if (null? l1) l1
      (append (reverse (cdr l1)) (list (car l1)))
      )
  )


