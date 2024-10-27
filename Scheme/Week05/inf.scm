#lang scheme
(define (assoc-rec key xs)
  (cond
    [(null? xs) #f]
    [(= (car (car xs)) key) (cdr (car xs))]
    [else
     (assoc-rec key (cdr xs))
     ]
    )
  
  )

(define (assoc-hop el xs)
  (foldr (lambda (cur res) (or (if (= (car cur) el) (cdr cur) #f ) res)) #f xs)
  )


(define (assoc-assoc el xs)
  (let
      (
       (res (assoc el xs))
       )
    (if (not res) res (cdr res))
      )
  )

(define (assoc-del el xs)
  (let
      (
       (filtered (filter (lambda (x) (not (= (car x) el))) xs))
       )
      
       (if (= (length filtered) (length xs)) xs
           filtered
           )
       
      )
  )

(define (replace xs assoc-xs)
     (cond [(null? xs) xs]
           [(not (assoc-rec (car xs) assoc-xs)) (cons (car xs) (replace (cdr xs) assoc-xs))]
           [else
            (cons (assoc-rec (car xs) assoc-xs) (replace (cdr xs) assoc-xs))]
    ))




(define (make-f-list xs func)
  (foldr (lambda (x res) (cons (cons x (func x)) res)) '() xs)
  )

(define (assoc-keys assoc-arr)
  (foldr (lambda (x rest) (cons (car x) rest)) '() assoc-arr)
  )
(define (assoc-values assoc-arr)
  (foldr (lambda (x rest) (cons (cdr x) rest)) '() assoc-arr)
  )

(define (zero-rows xss)
  (map (lambda (current-row)
         (if ( = (length current-row)
                 (length (filter (lambda (current-col)
                                   (not (= current-col 0))) current-row)
             ))
             current-row
             (map (lambda (dummy) 0) current-row)
         )
   ) xss)
  )

(define  (all? pred? arr)
  (cond
    [(null? arr) #t]
    [(not (pred? (car arr))) #f]
    [else
     (all? pred? (cdr arr))
     ]
  )); написах го защото мислех ,че ще ми трябва ама да стои

(define (transpose xss)
  (cond
    [(null? (car xss)) '()]
    [else
        (cons (map (lambda (current-row) (car current-row)) xss)
              (transpose
               (map (lambda (current-row) (cdr  current-row)) xss)
               )
              )
     ]
    )
  )

(define (zero-cols xss)
  (transpose (zero-rows (transpose xss))) ; dosta maloumno ama e krasivo ngl
  )

(define (main-diagonal xss)
  (define (iter i xss)
    (if (null? xss) '()
      (cons (list-ref (car xss) i) (iter (+ i 1) (cdr xss)) )
      )
    )
  (iter 0 xss)
  )
(define (secondary-diagonal xss)
  (define (iter i xss)
    (if (null? xss) '()
      (cons (list-ref (car xss) i) (iter (- i 1) (cdr xss)) )
      )
    )
  (iter (- (length xss) 1) xss)
  )

(define (diagonals xss)
  (cons (main-diagonal xss) (list (secondary-diagonal xss)))
  )



(define (triangular? xss)
  (define (iter xss i)
    (cond
      [(null? xss) #t]
      [(all? (lambda (y) (= y 0)) (take (car xss) i)) (iter (cdr xss) (+ i 1))]
      [else #f]
      )
  )
  (iter xss 0)
)
; след доста опити осъзнах че трябва да го направя итеративно по скоро
; този spiral matrix че нещо не ми идва рекурсията


(define (pos row col xss)
  (list-ref (list-ref xss row) col)
  )

(define (reverse-range b a)
  (if (<= b a) '()
      (cons b (reverse-range (- b 1) a))
      )
  )

(define (spiral xss)
  (define (additional-wrapper desired-length)
     (define (iter up down left right res)
       (if (= (length res) desired-length)
           res
           (let
               (
                [left-to-right (map (lambda (col-index) (pos up col-index xss)) (range left (+ 1 right)))]
                [up-to-down (map (lambda (row-index) (pos row-index right xss)) (range (+ 1 up) (+ 1 down)))]
                [right-to-left (map (lambda (current-col) (pos down current-col xss)) (reverse-range (- right 1) (- left 1)))]
                [down-to-up (map (lambda (current-row) (pos current-row left xss)) (reverse-range (- down 1) up))]
                )
             (iter (+ 1 up) (- down 1) (+ 1 left) (- right 1) (append res
                                                                       left-to-right
                                                                       up-to-down
                                                                       right-to-left
                                                                       down-to-up
                                                                       )
                   )
         
             )
       )
    )
    (iter 0 ( - (length xss) 1) 0 (- (length (car xss)) 1) '())
  ;на всяка итерация се маха един layer на квадрата
  )
  (additional-wrapper (* (length (car xss)) (length xss)))
  )
  ;tazi zadacha mi vze zdraveto







