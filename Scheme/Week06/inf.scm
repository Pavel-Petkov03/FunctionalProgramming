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
(define (at-level level tr)
  (define (iter current-level current-tr)
    (cond
      [(empty-tree? current-tr) '()]
      [(= current-level level) (list (root current-tr))]
      [else
       (append (iter (+ current-level 1) (left-tree current-tr))
               (iter (+ 1 current-level) (right-tree current-tr))
       )]
      )
    )
   (iter 0 tr)
    )

(define (map-tree f tr)
  (if (empty-tree? tr) tr
      (make-tree (f (root tr))
                 (map-tree f (left-tree tr))
                 (map-tree f (right-tree tr))
             
       )
   ))

(define (filter-tree pred? tr)
  (cond
    [(empty-tree? tr) '()]
    [(pred? (root tr))
     (append (list (root tr))
             (filter-tree pred? (left-tree tr))
             (filter-tree pred? (right-tree tr)))
             ]
    [else
     (append (filter-tree pred? (left-tree tr))
             (filter-tree pred? (right-tree tr)))
     ]
    
    )
  )

; в общи линии бинарните дървета са по - лесни от матриците

(define our-cool-graph '((1 . (2 3))
                         (2 . (3))
                         (3 . (4 5))
                         (4 . ())
                         (5 . (2 4 6))
                         (6 . (2))))
; като гледам графа е просто асоциативен масив , който представя таблица за съседство
; тоест ще използваме assoc-ref(взима value)


(define (get-v graph)
  (car (car graph))
  )
(define (get-v-links graph)
  (cdr (car graph))
  )

(define (graph-ref vertex graph)
  (cdr (assoc vertex graph))
  )

(define (edges graph)
  (if (null? graph) '()
      (append (map (lambda (x) (cons (get-v graph) x)) (get-v-links graph))
              (edges (cdr graph))
              )
      )
  )

(define (filter-children graph pred? vertex)
  (filter (lambda (x) (pred? x)) (graph-ref vertex graph))
  )


; относно задачата която е за махане на vertex
; доколкото разбирам искат резултата да не е graph защото ако махнем среден връх
; ще се раздели на поне два графа

(define (remove-vertex graph vert)
  (map
   (lambda (ls) (filter (lambda (element)  (not (= element vert))) ls))
   (filter (lambda (ls) (not (= (car ls) vert))) graph)
   )
  ); това е n^2 алгоритъм което е fine


; защото сме готини ще построим конструктивен предикат който:
; връща път ако намери
; #f ако няма

(define (append#f v deep wide)
  (cond
    [deep (append (list v) deep)]
    [wide (append (list v) wide)]
    [else #f]
    )
  )


(define (path graph v u)
    (define (recurse graph v current-lookup-arr trev)
    (cond
      [(member v trev) #f]
      [(null? current-lookup-arr) #f]
      [( = (car current-lookup-arr) u) (cons v (cons u '()))]
      [else
       (append#f
        v
        (path? (car current-lookup-arr) (graph-ref (car current-lookup-arr) graph) (cons v trev)) ;; deep lookup
        )
       ]
      )
    )
   (recurse v (graph-ref v graph) trev) )

; осъзнах че щом се решава с обхождане в широчина и дълбочина значи може
; да се направи биективна функция която мапва graph във deep list и да се обхожда аналогично



(define (path graph v u) (path? graph v u '()))





