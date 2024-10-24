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


(define (list-tail ls n); приемаме че ако ни прецакат и вкарат по - голямо число от
                          ; дължината на списъка ще хвърли грешка
  (if (= n 0) ls
      (list-tail (cdr ls) (- n 1))
      )
  )

(define (list-ref ls index)
  (if (= index 0) (car ls)
      (list-ref (cdr ls) (- index 1))
      )
  )

(define (member* pred? ls element)
  (cond [(null? ls) #f]
        [(pred? (car ls) element) ls]
        [else
         (member* pred? (cdr ls) element)
         ]
      
      )
  ); Конструктивен предикат -> предикат който връща false ако
                            ; липсва element но остатък от масива ако е true
                             ; and е конструктивен предикат , защото (and #t 5) -> 5
(define (member ls element)
  (member* equal? ls element)
  )
(define (memq ls element)
  (member* eq? ls element)
  )
(define (memv ls element)
  (member* eqv? ls element)
  )

; сега е момента да споменем разликата между equal? eqv? eq?
; equal? deep search(най - safe ама и най - бавно защото рекурсивно проверява по дървото)
; eq? проверява адресите на член данните
; eqv? проверява адресите на член данните плюс стойностите


(define (from-to a b); правя го inclusive
  (if (> a b) '()
      (cons a (from-to (+ a 1) b))
      )
  )

(define (collect a b next)
  (if (> a b) '()
      (cons a (collect (next a) b))
      )
  )
; racket имплементира map filter foldr foldl
; foldl не е ляво асоциативно затова си пиша сам foldl
; за да правя reverse алгоритмите по - лесно

(define (map ls func)
  (if (null? ls) ls
      (cons (func (car ls)) (map (cdr ls) func))
      )
  )

(define (filter ls pred?)
  (cond
    [(null? ls) ls]
    [(pred? (car ls)) (cons (car ls) (filter (cdr ls) pred?))]
    [else
     (filter (cdr ls) pred?)
     ]
    )
  )

(define (foldr op nv l) ; js -> reduce
  (if (null? l)
      nv
      (op (car l) (foldr op nv (cdr l)))
      )
  )

;можем да имплементираме map и filter с foldr

(define (map-with-foldr l func)
  (foldr (lambda (x r) (cons (func x) r))'() l)
  )

(define (filter-with-foldr l pred?)
  (foldr (lambda (x r)
           (if (pred? x) (cons x r) r)
           )
         '()
         l
         )
  )


(define (foldl op nv l)
  (if (null? l) nv
      (foldl op (op nv (car l)) (cdr l))
      )
  )

(define (umen-reverse l)'; това вече е 0(n)
  (foldl (lambda (x y) (cons y x)) '() l)
  )

;; имплементацията на foldl в racket e iter foldr
;; ако искаме да правим reverse алгоритми на листи ще изпозлваме foldl(нашия не на racket)


