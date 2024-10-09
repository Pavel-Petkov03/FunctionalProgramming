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