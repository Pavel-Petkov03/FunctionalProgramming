#lang racket
(define (tom room) (modulo (+ room 1) 3))
(define house '((0 1 2) (1 0 2) (2 0 1 3) (3 2)))


(define (get-neighboors graph node)
  (cond
    [(null? graph) node]
    [(= node (caar graph)) (cdar graph)]
    [else
     (get-neighboors (cdr graph) node)
     ]
    )
  )


(define (tom-moves-stream start-node tom-func)
  (stream-cons start-node (tom-moves-stream (tom-func start-node) tom-func))
  )

(define (foldr1 op l)
  (if (null? (cdr l)) (car l)
          (op (car l) (foldr1 op (cdr l)))))


(define (spike house spike-room tom-func tom-room)
  (define all-tom-moves (tom-moves-stream tom-room tom-func))
  (define (path n v times)
    (cond
      [(= n v) = (list v)]
      [(= times 0) `()]
      [else
       (let
           (
            [possible-paths (filter (lambda (ls) (not (null? ls)))  (map (lambda (next-room) (path next-room v (- times 1))) (cons n (get-neighboors house n))))])
         (if (null? possible-paths)
             `()
             (cons n (car possible-paths))
             ))]
    ))

  (define (iter tom-moves-stream times)
    (let
        (
        [generated-path (path (stream-first tom-moves-stream) spike-room times)]
        )
      (if (not (null? generated-path))
          generated-path
          (iter (stream-rest tom-moves-stream) (+ 1 times))
          )
  ))
  (if (= spike-room tom-room)
      '()
      (reverse (iter (stream-rest all-tom-moves) 1))
  ))

; btw (spike house 3 tom 0) -> '(3 2 2) koeto pak e vqrno
; nqmam vreme za bonusa ;)
; бонуса е лесен по принцип просто трябва да променя iter да работи с list вместо stream
; и да имам една функция която мести потока, докато iter мести листа


(define (set-list-value-at-index ls index val)
  (if (= index 0)
      (cons val (cdr ls))
      (cons (car ls) (set-list-value-at-index (cdr ls) (- index 1) val))
  ))

(define (fill-space res element)
  (cond
    [(null? res) `()]
    [(string=? (car res) "") (cons element (cdr res))]
    [else
     (fill-space (cdr res) element)
     ]
    )
  )

(define (wordle assoc-list)

  
  (define (iter assoc-list include-list exclude-list distribute-list generated-result)
    (if (null? assoc-list)
        (cons distribute-list generated-result)
        (let*
            ([state-result (inner-iter (caar assoc-list) (cadr assoc-list) include-list exclude-list distribute-list generated-result 0)])
          (if (= state-result "no solution")
              "no solution"
              (apply iter state-result)
           )
          )
        ))
  
  (define (inner-iter str ops include-list exclude-list distribute-list generated-result current-index)
    (let
        ([current-char (list-ref generated-result current-index)])
      
    (cond
      [(null? str) (list include-list exclude-list distribute-list generated-result)]
      [(char=? (car ops) #\+)
       (cond
         [(or (member (string-ref str 0) exclude-list) (and (not (= current-char "")) (not (= current-char (string-ref str 0)))))  "no solution"]
         [(member current-char distribute-list)
          (inner-iter (substring str 1) (substring ops 1) (cons current-char include-list) exclude-list (remove current-char distribute-list)
                       (set-list-value-at-index generated-result current-index current-char) ( + 1 current-index))
          ]
         [else
          (inner-iter (substring str 1) (substring ops 1) (cons current-char include-list) exclude-list distribute-list
                       (set-list-value-at-index generated-result current-index current-char) ( + 1 current-index))
          ]
         )
       ]
      [(char=? (car ops) #\-)
       (if (or (member (string-ref str 0) include-list) (member (string-ref str 0) generated-result) (member (string-ref str 0) distribute-list))
           "no solution"
           (inner-iter (substring str 1) (substring ops 1) include-list (cons (string-ref str 0) exclude-list) distribute-list
                       generated-result ( + 1 current-index))
           )
       ]
      [(char=? (car ops) #\?)
       (cond
         [(member (string-ref str 0) exclude-list) "no solution"]
         [(member (string-ref str 0) distribute-list)
          (inner-iter (substring str 1) (substring ops 1) include-list exclude-list distribute-list
                       generated-result ( + 1 current-index))
          ]
         [else
          (inner-iter (substring str 1) (substring ops 1) include-list exclude-list (cons current-char distribute-list)
                       generated-result ( + 1 current-index))
          ]
         )
       ]
    )))
  (define result-state (iter assoc-list '() '() '() '()))
         (if (string=? result-state "no solution")
             "no solution"
             (let*
               (
               (result (cdr result-state))
               (distribute-list (car result-state))
               (empty-spaces (filter (lambda (letter) (not (string=? letter ""))) result))
               (fills-spaces (length distribute-list))
               )
             (cond
               [(and (= empty-spaces 0) (= fills-spaces 0)) result]
               [(and (= empty-spaces 1) (= fills-spaces 1)) (fill-space result (car distribute-list))]
               [(> fills-spaces empty-spaces) "no solution"]
               [else
                "many solutions"]
     
               )
             )
  ))
; daje nqmam namerenie da produljavam s taq zadacha zashtoto mi e losho 
  
