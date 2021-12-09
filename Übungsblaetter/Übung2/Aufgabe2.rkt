#lang racket

(define xs (list 1 2 3 -4 5 6 7 8 -14))

;2.1
(define (absBetragList list) 
  (map abs list))

(absBetragList xs)

;2.2
(define (divisible-by? x)
  (zero? (remainder x 7)))

(filter divisible-by? xs)

;2.3
(define (Summe-rekursiv list)
   (cond[(empty? list) 0 ]
       [else  (+(first list) (Summe (rest list))) ]
   )
)

(define (Summe list)
  (foldl + 0 list))

(define (odd-and-greater-three? x)
  (> x 3))

(Summe(filter odd-and-greater-three? (filter odd? xs)))

;2.4


// Aufgaben Refactor
(define xs (list 1 -3 45 2 7 -14))

; Absolutbeträge einer Liste berechnen
(define (calcAbsList lst)
  (map abs lst))

(calcAbsList xs)


; Teilliste aller glatt durch 7 teilbaren Zahlen
(define (divisibleBy7 lst)
  (filter (lambda (x) (= 0 (remainder x 7))) lst))

(divisibleBy7 xs)

; Summe der ungeraden Zahlen > 3 in xs
(define (sumUneven lst)
  (foldr + 0 (filter (lambda (x) (and (> x 3)(odd? x))) lst))
)

(sumUneven xs)
