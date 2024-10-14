#lang scheme

; ще решавам само интересните задачи
(define (partition-function-wrapper current end gen accumulator)
  (define (partition-function-wrapper-looper i accumulator)
    ( if (= i end)
         accumulator
         ( partition-function-wrapper-looper (+ i 1)
                                           {cond
                                            [(= (+ i gen) end) (+ accumulator 1)]
                                            [ (> (+ i gen) end) accumulator]
                                            [else
                                                (partition-function-wrapper i end (+ i gen) accumulator)
                                             ]
                                            }
                                           )
           
    )
  )
  (partition-function-wrapper-looper current accumulator))



(define (partition-function n )
  (partition-function-wrapper 1 n 0 1)
  )




(define (get-num-len-wrapper number)
  (if (= number 0)
      0
      (+ 1 (get-num-len-wrapper (quotient number 10))
      )
  )
  )

(define (get-number-len number)
  (if (= number 0)
      1
      (get-num-len-wrapper number)
      )
  )

(define (get-number-index-wrapper whole-number searched iter-index)
  (if (or (= (remainder whole-number 10) searched) (= whole-number 0))
      iter-index
      (get-number-index-wrapper (quotient whole-number 10) searched (+ 1 iter-index))
      )
  )

(define (get-number-index whole-number searched)
  (get-number-index-wrapper whole-number searched 0)
  )


(define (is-number-found? whole-number searched)
  (not (= (get-number-index whole-number searched) (get-number-len whole-number)))
  )


(define (sort-n-iter n iter result number-len)
  (if (= n 0)
      result
      (let*
         {
          [current-index (get-number-index n iter)]
          [new-n (- n (* (expt 10 current-index) iter))]
          [new-result (+ result (* (expt 10 (- number-len iter)) iter))]
          }
         (if (= number-len current-index)
             (sort-n-iter n (+ 1 iter) result number-len)
             (sort-n-iter new-n iter new-result number-len)
             )
         )
      )
)
  
  
(define (sort-n n) ; ясно е че counting-sort e the best ама не мога да го използвам
                   ;защото нямам право на масиви а няма да вкарам 10 променливи
                   ; затова просто ще взимам индекса на най - голямото число ще го изваждам докато не стане n 0 и ще пълня друго число
  #t
  )


