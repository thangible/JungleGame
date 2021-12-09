#lang racket

;2.1

(define (laenge list)
  (if (empty? list) 0
      (+ 1 (laenge (cdr list))) 
  ))

;2.2

(define (fakultaet n)
  (cond [(equal? n 0) 1]
        [(equal? n 1) 1]
        [else (* n (fakultaet (- n 1)))]))
;2.3

(define (power r n)
  (cond [(equal? 0 n) 1]
        [(even? n) (* (power r (/ n 2)) (power r (/ n 2)))]
        [else (* r (power r  (- n 1)))]
))

;2.4
(define euler 1)
(for ([i (in-range 1 1000)])
    (set! euler (+ euler (/ 1 (fakultaet i))))
  )
(define erste1000 (round (* euler (power 10 1001))))

;2.5
(define (pi-stellen n)
   (cond[(equal? n 0) 4 ]
       [else   (+(pi-stellen (- n 1)) (* 4 (/(power -1 n) (+(* 2 n) 1))))]
   )
)

(define (pi-in-decimal n)
  (*(pi-stellen n) (power 10 n))
)


