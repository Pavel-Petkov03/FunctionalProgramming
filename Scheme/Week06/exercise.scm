#lang racket

(define (foldr op init l)
  (if (null? l)
      init
      (op (car l) (foldr op  init (cdr l)))))

(define (foldl op init l)
  (if (null? l)
      init
      (foldl op (op init (car l)) (cdr l))))



(define (get-number-len number)
  (define (recurse num iter)
    (if (= num 0) iter
        (recurse (quotient num 10) (+ iter 1))
        )
    )
  (if (= number 0) 1 (recurse number 0))
  )

(define (palindrome? num)
  (define (recurse num len)
    (cond
      [(or (= len 1) (= len 0)) #t]
      [(not (= (remainder num 10) (quotient num (expt 10 (- len 1))) )) #f]
      [else
       (recurse (quotient (remainder num (expt 10 (- len 1))) 10) (- len 2))
       ]
      )
    )
  (recurse num (get-number-len num))
  )


(define (from-to a b)
  (if (= a b) '()
      (cons a (from-to (+ a 1) b))
  ))

(define (count-palindromes a b)
  (foldr (lambda (x res) (if (palindrome? x) (+ 1 res) res)) 0 (from-to a (+ 1 b)))
  )

(define (is-prime? n)
  (define (iter cur-del)
    (cond
      [(= cur-del n) #t]
      [(= (remainder n cur-del) 0) #f]
      [else
       (iter (+ 1 cur-del))
       ]
      )
    )
  (iter 2)
  )

(define (sum-primes n k)
  (define (iter cur cur-n res)
    (cond
      [(= cur-n 0) res]
      [(is-prime? cur) (iter (+ 1 cur) (- cur-n 1) (+ res cur))]
      [else
       (iter (+ 1 cur) cur-n res)
       ]
      )
    )

  (iter k n 0)
  )


(define (prime-factors n)
  (define (iter cur-n cur-del res counter prev-del)
    (cond
      [(= (remainder cur-n cur-del) 0) (iter (/ cur-n cur-del) cur-del res (+ 1 counter) cur-del)]
      [(> counter 0) (iter cur-n cur-del (cons (cons prev-del counter) res) 0 prev-del)]
      [(= cur-n 1) res]
      [else
       (iter cur-n (+ 1 cur-del) res counter prev-del)
       ]
      )
    )
  (reverse (iter n 2 '() 0 "vse taq"))
  )


(define (inscreasing? l)
  (define (iter cur l)
    (if (null? l) #t
        (and (> (car l) cur) (iter (car l) (cdr l)))
    ))
  (if (null? l) "praznata redica ne mislq che e narastvashta ama nz"
      (iter (car l) (cdr l))
      )
  
  )


(define (progression? l)
  (define (iter dif l)
    (cond
      [(null? (cdr l)) #t]
      [(not (= (- (cadr l) (car l)) dif)) #f]
      [else
       (iter dif (cdr l))
       ]
      )
    )
    (let
        ([len (length l)])
        (cond
          [(= len 0) #f]
          [(= len 1) #t]
          [else
           (iter (- (cadr l) (car l)) l)
            ]
          )
        )
  )


(define (has-duplicates l)
  (define (recurse mem l)
    (cond
      [(null? l) #f]
      [(member (car l) mem) #t]
      [else
       (recurse (cons (car l) mem) (cdr l))
       ]
      )
    )
  (recurse '() l)
  )

(define (dedup l)
  (define (recurse mem l)
    (cond
      [(null? l) l]
      [(member (car l) mem) (recurse mem (cdr l))]
      [else
       (cons (car l) (recurse (cons (car l) mem) (cdr l)))
       ]
      )
    )
  (recurse '() l)
  )

(define (union l s)
  (dedup (append l s))
  )


(define (intersection l s)
  (cond
    [(null? l) l]
    [(member (car l) s) (cons (car l) (intersection (cdr l) s))]
    [else
      (intersection (cdr l) s)
     ]
    )
  )

(define (product l s)
  (foldr append '() (map
   (lambda (x)
     (map (lambda (y) (cons x y)) s)
     )
   l))
  );мизерно


(define (del-assoc alist key)
  (filter (lambda (x) (not (= (car x) key))) alist)
  )

(define (add-assoc alist key value)
  (cons (cons key value) (del-assoc alist key))
  )

(define (alist-values alist)
  (map (lambda (cur) (cdr cur)) alist)
  )

(define (list-to-count-assoc xs)
  (define (iter xs alist)
    (cond
      [(null? xs) alist]
      [(assoc (car xs) alist) (iter (cdr xs)
                                    (add-assoc alist (car xs)
                                               (+ 1 (cdr (assoc (car xs) alist)))
                                               ))]
      [else
        (iter (cdr xs) (add-assoc alist (car xs) 1))
       ]
      
      )
    )
  (iter xs '())
  )

(define (most-common l)
  (car (foldr (lambda (x r) (if (> (cdr x) (cdr r)) x r)) '("vse taq" . 0) (list-to-count-assoc l)))
  )

(define (scalar-product l m)
  (if (or (null? m) (null? m)) 0
      (+
       (* (car l) (car m))
       (scalar-product (cdr l) (cdr m))
       )
   )
  )


(define (transpose matrix)
  (apply map list matrix) ; mazno cheetche
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
  ); pisah gi predi i si gi kopvam prilejno

(define (diagonal-product xss)
  (scalar-product (main-diagonal xss) (secondary-diagonal xss))
  )


(define (matrix-multiply m t) ;приемаме че имаме коректен input
  (map (lambda (row)
         (map (lambda (col)
                (scalar-product row col)
                ) (apply map list t)
                  )
         ) 
           m);emi na edin red e(dori da e callback hell)
  )




(define (longest-inc-arr ls)
  (define (iter ls max max-index i len)
    (cond
      [(null? ls) (if (< max len) (cons (- i max) len) (cons max-index max))]
      [(and (not (null? (cdr ls))) (< (car ls) (cadr ls))) (iter (cdr ls) max max-index (+ i 1) (+ len 1))]
      [(< max len) (iter (cdr ls) len (- i len) (+ i 1) 1)]
      [else
       (iter (cdr ls) max max-index (+ i 1) 1)
       ]
      )
    )
  (iter ls 1 0 1 1)
  ); pisah go predi i si go kopvam prilejno

(define (sub-arr a b l)
  (define (iter i j ls)
    (cond
      [(< i a) (iter (+ 1 i) j (cdr ls))]
      [(or (null? ls) (> i j)) '()]
      [else
        (cons (car ls) (iter (+ 1 i) j (cdr ls)))
       ]
      )
    )
  (iter 0 b l)
  )

(define (max-ordered-sublist arr)
  (let*
      (
      (index-len-pair (longest-inc-arr arr))
      (start-index (car index-len-pair))
      (length (cdr index-len-pair))
      )
    
     (sub-arr start-index (- (+ start-index length ) 1) arr)
     
      )
  )


(define (replace ls alist)
  (cond
    [(null? ls) ls]
    [(assoc (car ls) alist) (cons (cdr(assoc (car ls) alist)) (replace (cdr ls) alist))]
    [else
     (cons (car ls) (replace (cdr ls) alist))
     ]
    )
  )


(define (distance p1 p2)
  (sqrt ( + (expt (- (car p1) (car p2)) 2) (expt (- (cdr p1) (cdr p2)) 2)) )
  )


(define (closest-point xs)
  (define (iter xs current-min-distance current-min-point point)
    (cond
      [(null? xs) current-min-point]
      [(< (distance point (car xs)) current-min-distance) (iter (cdr xs) (distance point (car xs)) (car xs) point)]
      [else
       (iter (cdr xs) current-min-distance current-min-point point)
       ]
      )
    )
  (lambda (point)
    (iter xs (distance (car xs) point) (car xs) point)
    )
  )











