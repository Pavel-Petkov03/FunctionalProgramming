#lang scheme


(define (my-identity)
  (lambda (something) something)
  )

(define (my-lambda procedure)
  (lambda (x) (procedure x))
  )

(define (negate-pred pred?)
  (lambda (x) (not (pred? x)))
  )

(define (my-compose fun1 fun2)
  (lambda (x) (fun1 (fun2 x)))
  )

#|
(= ((my-identity) 7) 7)
(equal? ((my-identity) "FMI") "FMI")

(= ((my-lambda identity) 7) 7)
(equal? ((my-lambda identity) "FMI") "FMI")
(= ((my-lambda string-length) "FMI") 3)

(equal? ((negate-pred even?) 6) #f)

(equal? ((my-compose even? string-length) "Tensorflow") #t)
(equal? ((my-compose (λ (x) (- x 5)) (λ (y) (+ y 25))) 5) 25)

; bachka ;)
|#

(define (complex-procedure unary-op binary-op )
  (lambda (x y) (binary-op (unary-op x) (unary-op y))) ;btw символът λ == lambda ама няма да си
                                                        ;вкарам гръцка клавиатура 
  )

(define (apply-n func n)
  (lambda (accumulator)
    (if (= n 0)
        accumulator
        (apply-n func (- n 1))
        )
    )
  )

