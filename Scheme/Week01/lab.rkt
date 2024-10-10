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

(define (get-roman-char number)
  (cond  ((= number 1000) "M")
         ((= number 900) "D")
           ((= number 500) "D")
           ((= number 400) "D")
           ((= number 100) "C")
           ((= number 50) "L")
           ((= number 500) "D")
           ((= number 10) "X")
           ((= number 5 ) "V")
           ((= number 1 ) "I")
           (else "Invalid input")
   )
  )

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





