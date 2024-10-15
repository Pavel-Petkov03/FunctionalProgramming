#lang scheme


(define (composite f g) (lambda (x) (f (g x)))) ; ((composite func1 func2) x)
(define repeated
  (lambda (n f x )
    (if (= n 1)
        (f x)
        (repeated (- n 1) f (f x)) ; за да ползвам ламбда рекурсивно не трябва
                                   ;главната функция да има аргументи
                                   ; защото не виждам как ще стане адекватно рекурсивно извикване
        )
    )
  )

(define repeate
  (lambda (n f x) (repeated n f x))
  )

(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a)
          (accumulate op nv (next a) b term next)))); polzvame tova ako iskame stack


(define (accumulate-i op nv a b term next) ; tova nqma stack ama e tail rec
  (if (> a b)
      nv
      (accumulate-i
        op
        (op nv (term a))
        (next a)
        b
        term
        next)))

(define (count p? a b)
  (accumulate-i + 0 a b
                (lambda (x) (if (p? x)  1 0))
                (lambda (x) (+ 1 x))
                )
  )

(define (any? p? a b)
  (accumulate-i equal?
                #f
                a
                b
                (lambda (x) (p? x))
                (lambda (x) (+ 1 x))
                
  ))


(define (all p? a b)
  (accumulate-i equal?
                #t
                a
                b
                (lambda (x) (p? x))
                (lambda (x) (+ 1 x))
                
  ))





