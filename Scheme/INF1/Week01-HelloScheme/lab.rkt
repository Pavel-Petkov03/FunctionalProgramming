#lang scheme
; почвам отзад напред


(define (help-preslava hours minutes songs)
  (if (= songs 0)
      (cons hours minutes)
      (cond 
             ((>= (+ minutes 3) 60) (help-preslava (+ hours 1) (remainder (+ minutes 3) 60) (- songs 1)))
              ((= hours 24) (help-preslava 0 minutes songs))
              (else
               (help-preslava hours (+ minutes 3) (- songs 1))
               )
            
      )
  ))


(define (number-to-roman number)
  (if (= number 0)
      ""
      (cond  ((>= number 1000) (string-append "M" (number-to-roman (- number 1000))))
             ((>= number 900) (string-append "CM" (number-to-roman (- number 900))))
             ((>= number 500) (string-append "D" (number-to-roman (- number 500))))
             ((>= number 400) (string-append "CD" (number-to-roman (- number 400))))
             ((>= number 100) (string-append "C" (number-to-roman (- number 100))))
             ((>= number 90) (string-append "XC" (number-to-roman (- number 90))))
             ((>= number 50) (string-append "L" (number-to-roman (- number 50))))
             ((>= number 40) (string-append "XL" (number-to-roman (- number 40))))
             ((>= number 10) (string-append "X" (number-to-roman (- number 10))))
             ((>= number 9) (string-append "IX" (number-to-roman (- number 9))))
             ((>= number 5) (string-append "V" (number-to-roman (- number 5))))
             ((>= number 4) (string-append "IV" (number-to-roman (- number 4))))
             ((>= number 1) (string-append "I" (number-to-roman (- number 1))))
   )
      )
  )


(define (mirror? number)
  (define (mirror-wrapper str len)
        (if (or (= len 0) (= len 1))
            #t
            (and (eq? (string-ref str 0) (string-ref str (- len 1)))
                 (mirror-wrapper (substring str 1 (- len 1)) (- len 2))
                 )
             )
            )
  (let (
         (str (number->string number))
      )
        (mirror-wrapper str (string-length str))
      )
        )


(define (help-students x y)
  (cond ( (and (= x 0) (= y 0)) "Center")
        (  (= x 0)  "Ordinate")
        (  (= y 0)  "Abscisa")
        ( (and (> x 0) (> y 0)) "1 Quadrant")
        ( (and (< x 0) (> y 0)) "2 Quadrant")
        ( (and (< x 0) (< y 0)) "3 Quadrant")
        ( (and (> x 0) (< y 0)) "4 Quadrant")
    )
  )


; надолу задачите са if else и няма много смисъл да ги пиша



