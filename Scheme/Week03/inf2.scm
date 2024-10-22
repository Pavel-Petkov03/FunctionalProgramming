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

(define (apply-n func iter)
  (lambda (x)
    (if (= iter 0)
        x
        ((apply-n func (- iter 1)) (func x)) ; правиш function call na
                                          ;apply-n и връща funcRef който се callва във (f x )
                                          ; явно е имало смислена функция с аргументи която прави
                                          ; рекурсивна lambda
                                          ; явно closure-а тук е по силен от този на js
                                          ;защото там това става само със смяна на среда (bind)
        )
    )
  )





; същите функции от exercise.lcm () (не ги импортвам защото не може да се импортне конкретна функция а цял файл :( )
; функциите са бинарни този път ама име оператори които са едновременно унарни и бинарни (* +)
(define (accumulate op nv a b term next)
    (if (> a b) nv
        (op (term a) (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))


(define (cool-expression num)
  (accumulate-i + 0 2 num (lambda (x) (expt x 3)) (lambda (x) (+ x 3)))
  )

(define (factoriel n)
  (accumulate-i * 1 0 n (lambda (x) (if (= x 0) 1 x)) (lambda (x) (+ x 1))) ; tova e cursed
  )

(define (logical-or a b)
  (or a b) ; тъй като or е специална форма не може да се подава като func ptr(wtf)
  )

(define (prime? num)
  (not (accumulate logical-or #f 2 (- num 1) (lambda (x) (= (remainder num x) 0)) (lambda (x) (+ x 1))))
  ; може да се напише като return само на accumualte без not отпред по закон на Де-Морган
  ; просто трябва да се махне not отпред да се сложи logical-and #t  вместо #f  и not  във първата ламбда
  
  )

; all and any  ги писах в другия файл

(define (argmin f a b)
  (accumulate-i (lambda (res cur)
                      (if (< (f res) (f cur))
                          res
                          cur
                          )
                    )
                  
                a
                (+ a 1)
                b
                (lambda (x) x)
                (lambda (x) (+ x 1))
                ) ; това е доста неоптимално защото всеки път смята текущата най малка стойност
                  ; ще помисля как да го оптимизирам използвайки accumulate
                  ; бих подавал pairs (k v) ама не се учи в тая лекция така че го зарязвам така
  );


(define (in-range? current a b)
  (and (>= current a) (<= current b))
  )


(define (count-pairs a b sum)
  (+ (if (= (remainder sum 2) 0)
         1 ; това е така защото не преброяваме два пъти при четно число и трябва да
           ; добавим едно че после да разделим накрая
         0
         )
     (quotient (accumulate +
              0
              a
              b
              (lambda (x)
                (if (in-range? (- sum x) a b )
                  1
                  0
                  ))
              (lambda (x) (+ x 1))
              ) 2))
  )
  

(define (count-digits number)
  (if (= number 0)
      1
      (accumulate-i
       +
       -1 ; прави do while все едно и затова махам едно от акумулатора
       number
       (+ number 1)
       (lambda (dummy1) 1)
       (lambda (a)
         (if (= a 0)
             (+ number 2) ; приключва цикъла
             (quotient a 10)
             )
         )
   )
      )
  )  





