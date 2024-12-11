#lang racket


; дървета
(define make-tree list)
(define root-tree car)
(define left-tree cadr)
(define right-tree caddr)
(define empty-tree? null?)
(define (leaf-tree? tree)
  (and (not (null? tree)) (null? (left-tree tree)) (null? (right-tree tree)))
  )

(define (pre-order tree)
  (if (empty-tree? tree)
      tree
      (append
       (list (root-tree tree))
       (pre-order (left-tree tree))
       (pre-order (right-tree tree))
       )
      )
  )

(define (in-order tree)
  (if (empty-tree? tree)
      tree
      (append
       (in-order (left-tree tree))
       (root-tree tree)
       (in-order (right-tree tree))
       )
      )
  )

(define (post-order tree)
  (if (empty-tree? tree)
      tree
      (append
       (post-order (left-tree tree))
       (post-order (right-tree tree))
       (list (root-tree tree))
       )
      )
  )


(define (map-tree f tree)
  (if (empty-tree? tree)
      tree
      (make-tree (f (root-tree tree)) (left-tree tree) (right-tree tree))
      )
  )



(define my-tree 
  (list 1 
        (list 2 
              (list 4 '() '()) 
              (list 5 '() '())) 
        (list 3 '() '())))


(define (height tree)
  (if (empty-tree? tree)
      0
      (+ 1
         (max
             (height (left-tree tree))
              (height (right-tree tree))
             )))
  )


(define (level tree n)
   (cond
     [(empty-tree? tree) tree]
     [(= n 0) (list (root-tree tree))]
     [else
      (append
        (level (left-tree tree) (- n 1))
        (level (right-tree tree) (- n 1))
        )
      ]))

(define (count-leaves tree)
  (if (leaf-tree? tree)
      1
      (+ (count-leaves (left-tree tree)) (count-leaves (right-tree tree))))
  )

(define (remove-leaves tree)
  (if (leaf-tree? tree)
      '()
      (make-tree (root-tree tree) (remove-leaves (left-tree tree)) (remove-leaves (right-tree tree)))))


(define (invert tree)
  (if (empty-tree? tree)
      tree
      (make-tree
          (root-tree tree)
          (invert (right-tree tree))
          (invert (left-tree tree))
          )))


(define (sorted? ls)
  (define (iter first rest)
    (if (null? rest)
        #t
        (and (< first (car rest)) (iter (car rest) (cdr rest))))
  )
  (if (null? ls) #t
      (iter (car ls) (cdr ls))
  ))


(define (bst? tree)
  (sorted? (in-order tree))
  )


(define (insert-bst x tree)
  (cond
    [(empty-tree? tree) (make-tree x '() '())]
    [(< x (root-tree tree) ) (make-tree (root-tree tree)(insert-bst x (left-tree tree)) (right-tree tree))]
    [(> x (root-tree tree)) (make-tree (root-tree tree) (left-tree tree) (insert-bst x (right-tree tree)))]))


(define (balanced? tree)
  (if (empty-tree? tree) #t
      (and ( <= (abs ( - (height (left-tree tree)) (height (right-tree tree)))) 1)
           (balanced? (left-tree tree)) (balanced? (right-tree tree))))
  )

; асоциативни списъци

(define (enumerate l)
  (define (iter l index)
    (if (null? l) l
        (cons (cons index (car l)) (iter (cdr l) (+ 1 index)))
        ))
  (iter l 0)
  )

(define (element-in-list? x lst)
  (cond
    ((null? lst) #f)
    ((equal? x (car lst)) #t)
    (else (element-in-list? x (cdr lst)))))



(define (get-unique l)
  (define (iter l res)
    (cond
      [(null? l) res]
      [(element-in-list? (car l) res) (iter (cdr l) res )]
      [else
       (iter (cdr l) (cons (car l) res))
       ]
    )
  )
  (iter l '())
  )


(define (histogram ls)
  (map (lambda (unique)
         (cons unique (foldl (lambda (cur acc) (if (= cur unique) (+ acc 1) acc)) 0 ls)
               ))
          (get-unique ls))
  )



(define (merge-assoc assoc1 assoc2 hash)
  (define (iter joined-assocs)
    (if (null? joined-assocs)
        `()
        (let
            ((assoc-pair (assoc (car joined-assocs) (cdr joined-assocs))))
            (if assoc-pair
                (cons
                 (cons (car joined-assocs) (hash (cadr joined-assocs) (cdr assoc-pair)))
                 (iter (cdr joined-assocs)))

                (cons
                 (cons (car joined-assocs) (cadr joined-assocs))
                 (iter (cdr joined-assocs))
                      
             )
            )
    )))
  (iter (append assoc1 assoc2) hash)
  )

(define (group-by f l)
  (map (lambda (unique) (filter (lambda (t) (= (f t) unique)) l))
  (get-unique (map f l))
  ))


(define run-length-encode histogram)


(define (generate-arr elem times)
   (if ( = times 0 )
       '()
       (cons elem (generate-arr elem (- times 1)))
))


(define (run-length-decode assoc-ls)
  (if (null? assoc-ls)
      '()
      (append (generate-arr (caar assoc-ls) (cdar assoc-ls)) (run-length-decode (cdr assoc-ls))
      )))
              


; generators

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream h t) (cons h (delay t)))))

(define head car)

(define (tail s) (force (cdr s)))

; stream-first and stream-rest

(define (take n stream)
  (if (= n 0)
      '()
      (cons (head stream) (take (- n 1) (tail stream)))))

(define (iterate f x)
  (cons-stream x (iterate f (f x ))
  ))


(define (nats-recurse elem)
    (cons-stream elem (nats-recurse (+ 1 elem))
  ))
(define nats
  (nats-recurse 1)
  )

(define (stream-map f s)
  (cons-stream ( f (head) s) (stream-map f (tail s))))

(define (from n)
  (cons-stream n (from (+ n 1))))

(define (stream-filter p? s)
  (if (p? (head s)) (cons-stream (head s) (stream-filter p? (tail s)))
                    (stream-filter p? (tail s))))

(define (nondivisor d) (lambda (n) (> (remainder n d) 0)))

(define (sieve stream)
  (cons-stream (head stream) (sieve (stream-filter (nondivisor (head stream)) (tail stream)))))

(define primes (sieve (from 2)))

