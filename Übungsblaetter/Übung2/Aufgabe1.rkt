#lang racket

;1.1 Es ist eine Funktion höherer Ordnung wenn eine Funktion als Parameter übergeben wird oder
; die Funktion eine Funktion zurückliefert


;1.2
;foldl
; Ist eine Funktion höherer Ordnung da es eine Funktion als Argument nimmt

;b
;(define (kopf-oder-schwanz x)
;(if (< x 0)
;car
;cdr))
;Ist eine Funktion höherer Ordnung da es eine Funktion als Rückgabe liefert

;c)
(define (pimento f arg1)
(lambda (arg2)
(f arg1 arg2)))
; Ist eine Funktion höherer Ordnung da es eine Funktion als Argument nimmt und liefert eine Funktion
; zurück

;d) (define (my-tan x)
;(/ (sin x) (cos x)))
; Ist keine Funktion höherer Ordnung da es keine Funktion als Argument nimmt und keine Funktion als
;Rückgabe liefert

;1.3
((pimento + 1) 3)
; 1 = arg1 und arg2 = 3 f = + arg2 hat nur den Gültigkeitsbreich innerhalb von lambda, arg 1 hat den
;Gültigkeitsbereich innerhalb der gesamten funktion

;1.4
(foldl (curry * 2) 1 '(1 2 3)) ; = 48 = 3*2*2*2*1*2  
(map cons '(1 2 3) '(1 2 3)) ;verknüpft die liste 123 und 123 als (1 1) (2 2) usw pair
(filter pair? '((a b ) () 1 (()))) ;durchsucht die Liste und gibt eine List mit pairs wieder
(map (compose (curry + 32) (curry * 1.8)) '(5505 100 0 -273.15)); Zunächst wird die Funktion
;F(x)= 32+1.8*x erstellt, diese Funktion wird dann auf jedes Listen Element angewendet










