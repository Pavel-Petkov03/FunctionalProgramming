#lang scheme
; двоични дървета -> (cons root left right)

(define (make-tree root left right) (list root left right))
(define (make-leaf x) (make-tree x '() '()))
(define (is-leaf? tr) (and (empty-tree? (left-tree tr)) (empty-tree? (right-tree tr))))
(define (left-tree tr) (cadr tr))
(define (right-tree tr) (caddr tr))
(define (empty-tree? tr) (null? tr))
(define (root tr) (car tr))

(define our-cool-tree '(1 (2 () ())
                          (3 (4 () ())
                             (5 () ()))))

(define (preorder tr)
  (if (empty-tree? tr)
      '()
      (append (list (root tr)) (preorder (left-tree tr)) (preorder (right-tree tr)))
      
      )
  )
(define (inorder tr)
  (if (empty-tree? tr)
      '()
      (append (inorder (left-tree tr)) (list (root tr)) (inorder (right-tree tr)))
      
      )
  )

(define (postorder tr)
  (if (empty-tree? tr)
      '()
      (append (postorder (left-tree tr)) (postorder (right-tree tr)) (list (root tr)))
      
      )
  )

; доста по - лесно е обхождането рекурсивно в тоя език

(define (size tr)
  (if (empty-tree? tr) 0
      (+ 1
         (size (left-tree tr))
         (size (right-tree tr))
         )
      )
  )

(define (height tr)
  (if (empty-tree? tr)
      0
      (+ 1
         (max
          (height (left-tree tr))
          (height (right-tree tr))
          )
         )
      )
  )

(define (balanced? tr)
  (or
   (empty-tree? tr)
   (let
       ((difference (abs (- (height (left-tree tr))
                        (height (right-tree tr))
                        ))
                    )
        )
     
     (and (or (= difference 1) (= difference 0))
          (balanced? (left-tree tr))
          (balanced? (right-tree tr))
          )
       )
   )
  )

(define (perfectly-balanced? tr)
  (or
   (empty-tree? tr)
   (let
       ((difference (abs (- (size (left-tree tr))
                        (size (right-tree tr))
                        ))
                    )
        )
     
     (and (or (= difference 1) (= difference 0))
          (perfectly-balanced? (left-tree tr))
          (perfectly-balanced? (right-tree tr))
          )
       )
   )
  )

;; може да се напише balanced която да приема предикат и да спестим малко писане
;; но истината е , че изглежда още по - трагично
;; така че го оставяме така ;(


(define (get-max-from-pred a b c pred?)
  (cond
    [(and (pred? a b) (pred? a c)) a]
    [(and (pred? b a) (pred? b c) b)]
    [else
     c
     ]
    )
  )


(define (min-tr pred? tr)
  (if (is-leaf? tr) (root tr)
          (get-max-from-pred (min-tr pred? (left-tree tr))
                             (min-tr pred? (right-tree tr))
                             (root tr)
                             pred?
                             )
          
      )
    )

; няма да пиша max защото е същото с обърнат предикат


(define (leaves tr)
  (cond
    [(empty-tree? tr) '()]
    [(is-leaf? tr) (list (root tr))]
    [else
     (append (leaves (left-tree tr)) (leaves (right-tree tr)))
     ]
    )
    )


